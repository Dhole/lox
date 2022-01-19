const std = @import("std");

const maxInt = std.math.maxInt;
const _scanner = @import("scanner.zig");
const _chunk = @import("chunk.zig");
const _token = @import("token.zig");
const _value = @import("value.zig");
const _common = @import("common.zig");
const _object = @import("object.zig");
const debug = @import("debug.zig");

const print = std.debug.print;

const OpCode = _common.OpCode;
const Value = _value.Value;
const Chunk = _chunk.Chunk;
const Scanner = _scanner.Scanner;
const Token = _token.Token;
const TT = _token.TokenType;
const Flags = _common.Flags;
const U8_COUNT = _common.U8_COUNT;
const copyString = _object.copyString;
const Obj = _object.Obj;
const ObjFunction = _object.ObjFunction;
const Objects = _object.Objects;

const Precedence = enum { //
NONE, //
ASSIGNMENT, // =
OR, // or
AND, // and
EQUALITY, // == !=
COMPARISON, // < > <= >=
TERM, // + -
FACTOR, // * /
UNARY, // ! -
CALL, // . ()
PRIMARY };

const Local = struct {
    name: Token,
    depth: isize,
    isCaptured: bool,
};

const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

const FunctionType = enum {
    FUNCTION,
    INITIALIZER,
    METHOD,
    SCRIPT,
};

pub const Compiler = struct {
    const Self = @This();

    enclosing: ?*Compiler,
    function: *ObjFunction,
    type: FunctionType,
    locals: [U8_COUNT]Local,
    localCount: usize,
    upvalues: [U8_COUNT]Upvalue,
    scopeDepth: usize,

    pub fn init(objects: *Objects, typ: FunctionType, enclosing: ?*Compiler) Self {
        var self = Self{
            .enclosing = enclosing,
            .function = ObjFunction.init(objects),
            .type = typ,
            .locals = undefined,
            .localCount = 0,
            .upvalues = undefined,
            .scopeDepth = 0,
        };
        var local = &self.locals[self.localCount];
        self.localCount += 1;
        local.depth = 0;
        local.isCaptured = false;
        if (typ != FunctionType.FUNCTION) {
            local.name.value = "this";
        } else {
            local.name.value = "";
        }

        return self;
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
};

pub fn Parser(comptime flags: Flags) type {
    return struct {
        const Self = @This();
        const rules = genRules();

        const ParseFn = fn (*Parser(flags), canAssign: bool) void;

        const ParseRule = struct {
            prefix: ?*const ParseFn,
            infix: ?*const ParseFn,
            precedence: Precedence,
        };

        current: Token,
        previous: Token,
        scanner: *Scanner,
        // current in the book
        compiler: *Compiler,
        compilerSet: bool,
        currentClass: ?*ClassCompiler,
        objects: *Objects,
        hadError: bool,
        panicMode: bool,

        pub fn init() Self {
            return Self{
                .current = undefined,
                .previous = undefined,
                .scanner = undefined,
                .compiler = undefined,
                .compilerSet = false,
                .currentClass = null,
                .objects = undefined,
                .hadError = false,
                .panicMode = false,
            };
        }

        pub fn compile(self: *Self, objects: *Objects, source: []const u8) ?*ObjFunction {
            self.objects = objects;
            self.hadError = false;
            self.panicMode = false;
            self.compiler = &Compiler.init(self.objects, FunctionType.SCRIPT, null);
            self.compilerSet = true;
            self.scanner = &Scanner.init(source);

            self.advance();
            while (!self.match(TT.EOF)) {
                self.declaration();
            }

            const fun = self.endCompiler();
            if (self.hadError) {
                return null;
            } else {
                return fun;
            }
        }

        pub fn markCompilerRoots(self: *Self) void {
            if (!self.compilerSet) {
                return;
            }
            var compiler: ?*Compiler = self.compiler;
            while (compiler) |cmp| {
                cmp.function.asObj().mark();
                compiler = cmp.enclosing;
            }
        }

        fn expression(self: *Self) void {
            self.parsePrecedence(Precedence.ASSIGNMENT);
        }

        fn block(self: *Self) void {
            while (!self.check(TT.RIGHT_BRACE) and !self.check(TT.EOF)) {
                self.declaration();
            }
            self.consume(TT.RIGHT_BRACE, "Expect '}' after block.");
        }

        fn function(self: *Self, typ: FunctionType) void {
            var compiler = &Compiler.init(self.objects, typ, self.compiler);
            compiler.function.name = self.objects.copyString(self.previous.value);
            self.compiler = compiler;
            self.beginScope();

            self.consume(TT.LEFT_PAREN, "Expect '(' after function name.");

            if (!self.check(TT.RIGHT_PAREN)) {
                while (true) {
                    self.compiler.function.arity += 1;
                    if (self.compiler.function.arity > 255) {
                        self.errAtCurrent("Can't have more than 255 parameters.");
                    }
                    const constant = self.parseVariable("Expect parameter name.");
                    self.defineVariable(constant);
                    if (!self.match(TT.COMMA)) {
                        break;
                    }
                }
            }

            self.consume(TT.RIGHT_PAREN, "Expect ')' after parameters.");
            self.consume(TT.LEFT_BRACE, "Expect '{' before function body.");
            self.block();

            const fun = self.endCompiler();
            self.emitBytes(@enumToInt(OpCode.CLOSURE), self.makeConstant(.{ .obj = fun.asObj() }));

            var i: usize = 0;
            while (i < fun.upvalueCount) : (i += 1) {
                self.emitByte(if (compiler.upvalues[i].isLocal) 1 else 0);
                self.emitByte(compiler.upvalues[i].index);
            }
        }

        fn method(self: *Self) void {
            self.consume(TT.IDENTIFIER, "Expect method name.");
            const constant = self.identifierConstant(&self.previous);

            var typ = FunctionType.METHOD;
            if (std.mem.eql(u8, self.previous.value, "init")) {
                typ = FunctionType.INITIALIZER;
            }
            self.function(typ);
            self.emitBytes(@enumToInt(OpCode.METHOD), constant);
        }

        fn classDeclaration(self: *Self) void {
            self.consume(TT.IDENTIFIER, "Expect class name.");
            const className = self.previous;
            const nameConstant = self.identifierConstant(&self.previous);
            self.declareVariable();

            self.emitBytes(@enumToInt(OpCode.CLASS), nameConstant);
            self.defineVariable(nameConstant);

            var classCompiler: ClassCompiler = undefined;
            classCompiler.enclosing = self.currentClass;
            self.currentClass = &classCompiler;

            self.namedVariable(className, false);

            self.consume(TT.LEFT_BRACE, "Expect '{' before class body.");
            while (!self.check(TT.RIGHT_BRACE) and !self.check(TT.EOF)) {
                self.method();
            }
            self.consume(TT.RIGHT_BRACE, "Expect '}' after class body.");
            self.emitByte(@enumToInt(OpCode.POP));

            self.currentClass = self.currentClass.?.enclosing;
        }

        fn funDeclaration(self: *Self) void {
            const global = self.parseVariable("Expect function name.");
            self.markInitialized();
            self.function(FunctionType.FUNCTION);
            self.defineVariable(global);
        }

        fn varDeclaration(self: *Self) void {
            const global = self.parseVariable("Expect variable name.");
            if (self.match(TT.EQUAL)) {
                self.expression();
            } else {
                self.emitByte(@enumToInt(OpCode.NIL));
            }
            self.consume(TT.SEMICOLON, "Expect ';' after variable declaration.");
            self.defineVariable(global);
        }

        fn expressionStatement(self: *Self) void {
            self.expression();
            self.consume(TT.SEMICOLON, "Expect ';' after value.");
            self.emitByte(@enumToInt(OpCode.POP));
        }

        fn forStatement(self: *Self) void {
            self.beginScope();
            self.consume(TT.LEFT_PAREN, "Expect '(' after 'for'.");
            if (self.match(TT.SEMICOLON)) {
                // No initializer.
            } else if (self.match(TT.VAR)) {
                self.varDeclaration();
            } else {
                self.expressionStatement();
            }

            var loopStart = self.currentChunk().count;
            var exitJump: isize = -1;
            if (!self.match(TT.SEMICOLON)) {
                self.expression();
                self.consume(TT.SEMICOLON, "Expect ';' after loop condition.");

                // Jump out of the loop if the condition is false.
                exitJump = @intCast(isize, self.emitJump(OpCode.JUMP_IF_FALSE));
                self.emitByte(@enumToInt(OpCode.POP)); // Condition.
            }

            if (!self.match(TT.RIGHT_PAREN)) {
                const bodyJump = self.emitJump(OpCode.JUMP);
                const incrementStart = self.currentChunk().count;
                self.expression();
                self.emitByte(@enumToInt(OpCode.POP));
                self.consume(TT.RIGHT_PAREN, "Expect ')' after for clauses.");

                self.emitLoop(loopStart);
                loopStart = incrementStart;
                self.patchJump(bodyJump);
            }

            self.statement();
            self.emitLoop(loopStart);

            if (exitJump != -1) {
                self.patchJump(@intCast(usize, exitJump));
                self.emitByte(@enumToInt(OpCode.POP)); // Condition
            }

            self.endScope();
        }

        fn ifStatement(self: *Self) void {
            self.consume(TT.LEFT_PAREN, "Expect '(' after 'if'.");
            self.expression();
            self.consume(TT.RIGHT_PAREN, "Expect ')' after condition.");

            const thenJump = self.emitJump(OpCode.JUMP_IF_FALSE);
            self.emitByte(@enumToInt(OpCode.POP));
            self.statement();
            const elseJump = self.emitJump(OpCode.JUMP);
            self.patchJump(thenJump);
            self.emitByte(@enumToInt(OpCode.POP));

            if (self.match(TT.ELSE)) {
                self.statement();
            }
            self.patchJump(elseJump);
        }

        fn printStatement(self: *Self) void {
            self.expression();
            self.consume(TT.SEMICOLON, "Expect ';' after value.");
            self.emitByte(@enumToInt(OpCode.PRINT));
        }

        fn returnStatement(self: *Self) void {
            if (self.compiler.type == FunctionType.SCRIPT) {
                self.err("Can't return from top-level code.");
            } else if (self.compiler.type == FunctionType.INITIALIZER) {
                self.err("Can't return a value from an initializer.");
            }

            if (self.match(TT.SEMICOLON)) {
                self.emitReturn();
            } else {
                self.expression();
                self.consume(TT.SEMICOLON, "Expect ';' after return value.");
                self.emitByte(@enumToInt(OpCode.RETURN));
            }
        }

        fn whileStatement(self: *Self) void {
            const loopStart = self.currentChunk().count;
            self.consume(TT.LEFT_PAREN, "Expect '(' after 'while'.");
            self.expression();
            self.consume(TT.RIGHT_PAREN, "Expect ')' after condition.");

            const exitJump = self.emitJump(OpCode.JUMP_IF_FALSE);
            self.emitByte(@enumToInt(OpCode.POP));
            self.statement();
            self.emitLoop(loopStart);

            self.patchJump(exitJump);
            self.emitByte(@enumToInt(OpCode.POP));
        }

        fn synchronize(self: *Self) void {
            self.panicMode = false;

            while (self.current.type != TT.EOF) {
                if (self.previous.type == TT.SEMICOLON) {
                    return;
                }
                switch (self.current.type) {
                    TT.CLASS, TT.FUN, TT.VAR, TT.FOR, TT.IF, TT.WHILE, TT.PRINT, TT.RETURN => return,
                    else => {},
                }
            }
            self.advance();
        }

        fn declaration(self: *Self) void {
            if (self.match(TT.CLASS)) {
                self.classDeclaration();
            } else if (self.match(TT.FUN)) {
                self.funDeclaration();
            } else if (self.match(TT.VAR)) {
                self.varDeclaration();
            } else {
                self.statement();
            }
            if (self.panicMode) {
                self.synchronize();
            }
        }

        fn statement(self: *Self) void {
            if (self.match(TT.PRINT)) {
                self.printStatement();
            } else if (self.match(TT.FOR)) {
                self.forStatement();
            } else if (self.match(TT.IF)) {
                self.ifStatement();
            } else if (self.match(TT.RETURN)) {
                self.returnStatement();
            } else if (self.match(TT.WHILE)) {
                self.whileStatement();
            } else if (self.match(TT.LEFT_BRACE)) {
                self.beginScope();
                self.block();
                self.endScope();
            } else {
                self.expressionStatement();
            }
        }

        fn currentChunk(self: *Self) *Chunk {
            return &self.compiler.function.chunk;
        }

        pub fn advance(self: *Self) void {
            self.previous = self.current;

            while (true) {
                self.current = self.scanner.scanToken();
                if (self.current.type != TT.ERROR) {
                    break;
                }

                self.errAtCurrent(self.current.value);
            }
        }

        fn consume(self: *Self, typ: TT, message: []const u8) void {
            if (self.current.type == typ) {
                self.advance();
                return;
            }
            self.errAtCurrent(message);
        }

        fn check(self: *Self, typ: TT) bool {
            return self.current.type == typ;
        }

        fn match(self: *Self, typ: TT) bool {
            if (!self.check(typ)) {
                return false;
            }
            self.advance();
            return true;
        }

        fn emitByte(self: *Self, byte: u8) void {
            self.currentChunk().write(byte, self.previous.line);
        }

        fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
            self.emitByte(byte1);
            self.emitByte(byte2);
        }

        fn emitLoop(self: *Self, loopStart: usize) void {
            self.emitByte(@enumToInt(OpCode.LOOP));

            const offset = self.currentChunk().count - loopStart + 2;
            if (offset > std.math.maxInt(u16)) {
                self.err("Loop body too large.");
            }

            self.emitByte(@intCast(u8, (offset >> 8) & 0xff));
            self.emitByte(@intCast(u8, offset & 0xff));
        }

        fn emitJump(self: *Self, instruction: OpCode) usize {
            self.emitByte(@enumToInt(instruction));
            self.emitByte(0xff);
            self.emitByte(0xff);
            return self.currentChunk().count - 2;
        }

        fn emitReturn(self: *Self) void {
            if (self.compiler.type == FunctionType.INITIALIZER) {
                self.emitBytes(@enumToInt(OpCode.GET_LOCAL), 0);
            } else {
                self.emitByte(@enumToInt(OpCode.NIL));
            }
            self.emitByte(@enumToInt(OpCode.RETURN));
        }

        fn emitConstant(self: *Self, value: Value) void {
            self.emitBytes(@enumToInt(OpCode.CONSTANT), self.makeConstant(value));
        }

        fn patchJump(self: *Self, offset: usize) void {
            // -2 to adjust for the bytecode for the jump offset itself.
            const jump = self.currentChunk().count - offset - 2;

            if (jump > std.math.maxInt(u16)) {
                self.err("Too much code to jump over.");
            }

            self.currentChunk().code[offset] = @intCast(u8, (jump >> 8) & 0xff);
            self.currentChunk().code[offset + 1] = @intCast(u8, jump & 0xff);
        }

        fn makeConstant(self: *Self, value: Value) u8 {
            const constant = self.currentChunk().addConstant(value);
            if (constant > std.math.maxInt(u8)) {
                self.err("Too many constants in one chunk.");
                return 0;
            }
            return @intCast(u8, constant);
        }

        fn endCompiler(self: *Self) *ObjFunction {
            self.emitReturn();
            const fun = self.compiler.function;
            if (flags.debugPrintCode) {
                if (!self.hadError) {
                    var name: []const u8 = "<script>";
                    if (fun.name) |n| {
                        name = n.chars;
                    }
                    debug.disassembleChunk(self.currentChunk(), name);
                }
            }
            if (self.compiler.enclosing) |compiler| {
                self.compiler = compiler;
            }
            return fun;
        }

        fn beginScope(self: *Self) void {
            self.compiler.scopeDepth += 1;
        }

        fn endScope(self: *Self) void {
            self.compiler.scopeDepth -= 1;

            while (self.compiler.localCount > 0 and
                self.compiler.locals[self.compiler.localCount - 1].depth > self.compiler.scopeDepth)
            {
                if (self.compiler.locals[self.compiler.localCount - 1].isCaptured) {
                    self.emitByte(@enumToInt(OpCode.CLOSE_UPVALUE));
                } else {
                    self.emitByte(@enumToInt(OpCode.POP));
                }
                self.compiler.localCount -= 1;
            }
        }

        fn binary(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const operatorType = self.previous.type;
            const rule = getRule(operatorType);
            self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

            switch (operatorType) {
                TT.BANG_EQUAL => self.emitBytes(@enumToInt(OpCode.EQUAL), @enumToInt(OpCode.NOT)),
                TT.EQUAL_EQUAL => self.emitByte(@enumToInt(OpCode.EQUAL)),
                TT.GREATER => self.emitByte(@enumToInt(OpCode.GREATER)),
                TT.GREATER_EQUAL => self.emitBytes(@enumToInt(OpCode.LESS), @enumToInt(OpCode.NOT)),
                TT.LESS => self.emitByte(@enumToInt(OpCode.LESS)),
                TT.LESS_EQUAL => self.emitBytes(@enumToInt(OpCode.GREATER), @enumToInt(OpCode.NOT)),
                TT.PLUS => self.emitByte(@enumToInt(OpCode.ADD)),
                TT.MINUS => self.emitByte(@enumToInt(OpCode.SUBTRACT)),
                TT.STAR => self.emitByte(@enumToInt(OpCode.MULTIPLY)),
                TT.SLASH => self.emitByte(@enumToInt(OpCode.DIVIDE)),
                else => unreachable, // Unreachable.
            }
        }

        fn call(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const argCount = self.argumentList();
            self.emitBytes(@enumToInt(OpCode.CALL), argCount);
        }

        fn dot(self: *Self, canAssign: bool) void {
            self.consume(TT.IDENTIFIER, "Expect property name after '.'.");
            const name = self.identifierConstant(&self.previous);

            if (canAssign and self.match(TT.EQUAL)) {
                self.expression();
                self.emitBytes(@enumToInt(OpCode.SET_PROPERTY), name);
            } else if (self.match(TT.LEFT_PAREN)) {
                const argCount = self.argumentList();
                self.emitBytes(@enumToInt(OpCode.INVOKE), name);
                self.emitByte(argCount);
            } else {
                self.emitBytes(@enumToInt(OpCode.GET_PROPERTY), name);
            }
        }

        fn literal(self: *Self, canAssign: bool) void {
            _ = canAssign;
            switch (self.previous.type) {
                TT.FALSE => self.emitByte(@enumToInt(OpCode.FALSE)),
                TT.NIL => self.emitByte(@enumToInt(OpCode.NIL)),
                TT.TRUE => self.emitByte(@enumToInt(OpCode.TRUE)),
                else => unreachable,
            }
        }

        fn grouping(self: *Self, canAssign: bool) void {
            _ = canAssign;
            self.expression();
            self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
        }

        fn number(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const value = std.fmt.parseFloat(f64, self.previous.value) catch |e| {
                std.debug.panic("{}", .{e});
            };
            _ = self.emitConstant(.{ .number = value });
        }

        fn or_(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const elseJump = self.emitJump(OpCode.JUMP_IF_FALSE);
            const endJump = self.emitJump(OpCode.JUMP);

            self.patchJump(elseJump);
            self.emitByte(@enumToInt(OpCode.POP));

            self.parsePrecedence(Precedence.OR);
            self.patchJump(endJump);
        }

        fn string(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const objString = self.objects.copyString(self.previous.value[1 .. self.previous.value.len - 1]);
            _ = self.emitConstant(.{ .obj = objString.asObj() });
        }

        fn namedVariable(self: *Self, name: Token, canAssign: bool) void {
            var getOp: u8 = undefined;
            var setOp: u8 = undefined;
            var arg = self.resolveLocal(self.compiler, &name);
            if (arg != -1) {
                getOp = @enumToInt(OpCode.GET_LOCAL);
                setOp = @enumToInt(OpCode.SET_LOCAL);
            } else {
                arg = self.resolveUpvalue(self.compiler, &name);
                if (arg != -1) {
                    getOp = @enumToInt(OpCode.GET_UPVALUE);
                    setOp = @enumToInt(OpCode.SET_UPVALUE);
                } else {
                    arg = self.identifierConstant(&name);
                    getOp = @enumToInt(OpCode.GET_GLOBAL);
                    setOp = @enumToInt(OpCode.SET_GLOBAL);
                }
            }

            if (canAssign and self.match(TT.EQUAL)) {
                self.expression();
                self.emitBytes(setOp, @intCast(u8, arg));
            } else {
                self.emitBytes(getOp, @intCast(u8, arg));
            }
        }

        fn variable(self: *Self, canAssign: bool) void {
            self.namedVariable(self.previous, canAssign);
        }

        fn this(self: *Self, canAssign: bool) void {
            if (self.currentClass == null) {
                self.err("Can't use 'this' outside of a class.");
                return;
            }
            _ = canAssign;
            self.variable(false);
        }

        fn unary(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const operatorType = self.previous.type;
            // Compile the operand.
            self.parsePrecedence(Precedence.UNARY);

            // Emit the operator instruction.
            switch (operatorType) {
                TT.BANG => self.emitByte(@enumToInt(OpCode.NOT)),
                TT.MINUS => self.emitByte(@enumToInt(OpCode.NEGATE)),
                else => unreachable, // Unreachable.
            }
        }

        fn parsePrecedence(self: *Self, precedence: Precedence) void {
            self.advance();
            const prefixRule = getRule(self.previous.type).prefix;
            const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.ASSIGNMENT);
            if (prefixRule) |rule| {
                rule.*(self, canAssign);
            } else {
                self.err("Expect expression.");
                return;
            }

            while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.type).precedence)) {
                self.advance();
                const infixRule = getRule(self.previous.type).infix;
                if (infixRule) |rule| {
                    rule.*(self, canAssign);
                } else {
                    unreachable;
                }
            }

            if (canAssign and self.match(TT.EQUAL)) {
                self.err("Invalid assignment target.");
            }
        }

        fn identifierConstant(self: *Self, name: *const Token) u8 {
            return self.makeConstant(.{ .obj = self.objects.copyString(name.value).asObj() });
        }

        fn identifiersEqual(a: *const Token, b: *const Token) bool {
            if (a.value.len != b.value.len) {
                return false;
            }
            return std.mem.eql(u8, a.value, b.value);
        }

        fn resolveLocal(self: *Self, compiler: *Compiler, name: *const Token) isize {
            var i = @intCast(isize, compiler.localCount) - 1;
            while (i >= 0) : (i -= 1) {
                const local = compiler.locals[@intCast(usize, i)];
                if (identifiersEqual(name, &local.name)) {
                    if (local.depth == -1) {
                        self.err("Can't read local variable in its own initializer.");
                    }
                    return i;
                }
            }
            return -1;
        }

        fn addUpvalue(self: *Self, compiler: *Compiler, index: u8, isLocal: bool) usize {
            _ = self;
            const upvalueCount = compiler.function.upvalueCount;

            var i: usize = 0;
            while (i < upvalueCount) : (i += 1) {
                const upvalue = compiler.upvalues[i];
                if (upvalue.index == index and upvalue.isLocal == isLocal) {
                    return i;
                }
            }

            if (upvalueCount == U8_COUNT) {
                self.err("Too many closure variables in function.");
            }

            compiler.upvalues[upvalueCount].isLocal = isLocal;
            compiler.upvalues[upvalueCount].index = index;
            compiler.function.upvalueCount += 1;
            return upvalueCount;
        }

        fn resolveUpvalue(self: *Self, compiler: *Compiler, name: *const Token) isize {
            if (compiler.enclosing) |enclosing| {
                const local = self.resolveLocal(enclosing, name);
                if (local != -1) {
                    enclosing.locals[@intCast(usize, local)].isCaptured = true;
                    return @intCast(isize, self.addUpvalue(compiler, @intCast(u8, local), true));
                }
                const upvalue = self.resolveUpvalue(enclosing, name);
                if (upvalue != 1) {
                    return @intCast(isize, self.addUpvalue(compiler, @intCast(u8, upvalue), false));
                }
                return -1;
            } else {
                return -1;
            }
        }

        fn addLocal(self: *Self, name: Token) void {
            if (self.compiler.localCount == U8_COUNT) {
                self.err("Too many local variables in function.");
                return;
            }

            var local = &self.compiler.locals[self.compiler.localCount];
            self.compiler.localCount += 1;
            local.name = name;
            local.depth = -1;
            local.isCaptured = false;
        }

        fn declareVariable(self: *Self) void {
            if (self.compiler.scopeDepth == 0) {
                return;
            }
            const name = &self.previous;
            var i = @intCast(isize, self.compiler.localCount) - 1;
            while (i >= 0) : (i -= 1) {
                const local = &self.compiler.locals[@intCast(usize, i)];
                if (local.depth != -1 and local.depth < self.compiler.scopeDepth) {
                    break;
                }

                if (identifiersEqual(name, &local.name)) {
                    self.err("Already a variable with this name in this scope.");
                }
            }

            self.addLocal(name.*);
        }

        fn parseVariable(self: *Self, errorMessage: []const u8) u8 {
            self.consume(TT.IDENTIFIER, errorMessage);
            self.declareVariable();
            if (self.compiler.scopeDepth > 0) {
                return 0;
            }
            return self.identifierConstant(&self.previous);
        }

        fn markInitialized(self: *Self) void {
            if (self.compiler.scopeDepth == 0) {
                return;
            }
            self.compiler.locals[self.compiler.localCount - 1].depth =
                @intCast(isize, self.compiler.scopeDepth);
        }

        fn defineVariable(self: *Self, global: u8) void {
            if (self.compiler.scopeDepth > 0) {
                self.markInitialized();
                return;
            }
            self.emitBytes(@enumToInt(OpCode.DEFINE_GLOBAL), global);
        }

        fn argumentList(self: *Self) u8 {
            var argCount: u8 = 0;
            if (!self.check(TT.RIGHT_PAREN)) {
                while (true) {
                    self.expression();
                    if (argCount == 255) {
                        self.err("Can't have more than 255 arguments.");
                    }
                    argCount += 1;
                    if (!self.match(TT.COMMA)) {
                        break;
                    }
                }
            }
            self.consume(TT.RIGHT_PAREN, "Expect ')' after arguments.");
            return argCount;
        }

        fn and_(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const endJump = self.emitJump(OpCode.JUMP_IF_FALSE);
            self.emitByte(@enumToInt(OpCode.POP));
            self.parsePrecedence(Precedence.AND);
            self.patchJump(endJump);
        }

        fn getRule(typ: TT) *ParseRule {
            return &Self.rules[@enumToInt(typ)];
        }

        fn errAtCurrent(self: *Self, message: []const u8) void {
            self.errAt(&self.current, message);
        }

        fn err(self: *Self, message: []const u8) void {
            self.errAt(&self.previous, message);
        }

        fn errAt(self: *Self, token: *Token, message: []const u8) void {
            if (self.panicMode) {
                return;
            }
            self.panicMode = true;
            std.log.err("[line {d}] Error", .{token.line});

            if (token.type == TT.EOF) {
                std.log.err(" at end", .{});
            } else if (token.type == TT.ERROR) {
                // Nothing.
            } else {
                std.log.err(" at '{s}'", .{token.value});
            }

            std.log.err(": {s}\n", .{message});
            self.hadError = true;
        }

        fn genRules() []ParseRule {
            var _rules: [@enumToInt(TT.ERROR) + 1]ParseRule = undefined;
            _rules[@enumToInt(TT.LEFT_PAREN)] = .{ .prefix = &grouping, .infix = &call, .precedence = Precedence.CALL };
            _rules[@enumToInt(TT.RIGHT_PAREN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.LEFT_BRACE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RIGHT_BRACE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.COMMA)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.DOT)] = .{ .prefix = null, .infix = &dot, .precedence = Precedence.CALL };
            _rules[@enumToInt(TT.MINUS)] = .{ .prefix = &unary, .infix = &binary, .precedence = Precedence.TERM };
            _rules[@enumToInt(TT.PLUS)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.TERM };
            _rules[@enumToInt(TT.SEMICOLON)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.SLASH)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.FACTOR };
            _rules[@enumToInt(TT.STAR)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.FACTOR };
            _rules[@enumToInt(TT.BANG)] = .{ .prefix = &unary, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.BANG_EQUAL)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.EQUALITY };
            _rules[@enumToInt(TT.EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EQUAL_EQUAL)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.EQUALITY };
            _rules[@enumToInt(TT.GREATER)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.COMPARISON };
            _rules[@enumToInt(TT.GREATER_EQUAL)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.COMPARISON };
            _rules[@enumToInt(TT.LESS)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.COMPARISON };
            _rules[@enumToInt(TT.LESS_EQUAL)] = .{ .prefix = null, .infix = &binary, .precedence = Precedence.COMPARISON };
            _rules[@enumToInt(TT.IDENTIFIER)] = .{ .prefix = &variable, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.STRING)] = .{ .prefix = &string, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.NUMBER)] = .{ .prefix = &number, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.AND)] = .{ .prefix = null, .infix = &and_, .precedence = Precedence.AND };
            _rules[@enumToInt(TT.CLASS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ELSE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FALSE)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FOR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FUN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.IF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.NIL)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.OR)] = .{ .prefix = null, .infix = &or_, .precedence = Precedence.OR };
            _rules[@enumToInt(TT.PRINT)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RETURN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.SUPER)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.THIS)] = .{ .prefix = &this, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.TRUE)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.VAR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.WHILE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ERROR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EOF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            return _rules[0..];
        }
    };
}
