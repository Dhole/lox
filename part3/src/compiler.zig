const std = @import("std");

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
const copyString = _object.copyString;
const Obj = _object.Obj;
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
        compilingChunk: *Chunk,
        scanner: *Scanner,
        objects: *Objects,
        hadError: bool,
        panicMode: bool,

        pub fn init() Self {
            return Self{
                .current = undefined,
                .previous = undefined,
                .compilingChunk = undefined,
                .scanner = undefined,
                .objects = undefined,
                .hadError = false,
                .panicMode = false,
            };
        }

        pub fn compile(self: *Self, objects: *Objects, source: []const u8, chunk: *Chunk) bool {
            self.compilingChunk = chunk;
            self.hadError = false;
            self.panicMode = false;
            self.scanner = &Scanner.init(source);
            self.objects = objects;

            self.advance();
            while (!self.match(TT.EOF)) {
                self.declaration();
            }
            self.endCompiler();
            return !self.hadError;
        }

        fn expression(self: *Self) void {
            self.parsePrecedence(Precedence.ASSIGNMENT);
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

        fn printStatement(self: *Self) void {
            self.expression();
            self.consume(TT.SEMICOLON, "Expect ';' after value.");
            self.emitByte(@enumToInt(OpCode.PRINT));
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
            if (self.match(TT.VAR)) {
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
            } else {
                self.expressionStatement();
            }
        }

        fn currentChunk(self: *Self) *Chunk {
            return self.compilingChunk;
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

        fn emitReturn(self: *Self) void {
            self.emitByte(@enumToInt(OpCode.RETURN));
        }

        fn emitConstant(self: *Self, value: Value) void {
            self.emitBytes(@enumToInt(OpCode.CONSTANT), self.makeConstant(value));
        }

        fn makeConstant(self: *Self, value: Value) u8 {
            const constant = self.currentChunk().addConstant(value);
            if (constant > std.math.maxInt(u8)) {
                self.err("Too many constants in one chunk.");
                return 0;
            }
            return @intCast(u8, constant);
        }

        fn endCompiler(self: *Self) void {
            self.emitReturn();
            if (flags.debugPrintCode) {
                if (!self.hadError) {
                    debug.disassembleChunk(self.currentChunk(), "code");
                }
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

        fn string(self: *Self, canAssign: bool) void {
            _ = canAssign;
            const objString = self.objects.copyString(self.previous.value[1 .. self.previous.value.len - 1]);
            _ = self.emitConstant(.{ .obj = objString.asObj() });
        }

        fn namedVariable(self: *Self, name: Token, canAssign: bool) void {
            const arg = self.identifierConstant(&name);
            if (canAssign and self.match(TT.EQUAL)) {
                self.expression();
                self.emitBytes(@enumToInt(OpCode.SET_GLOBAL), arg);
            } else {
                self.emitBytes(@enumToInt(OpCode.GET_GLOBAL), arg);
            }
        }

        fn variable(self: *Self, canAssign: bool) void {
            self.namedVariable(self.previous, canAssign);
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

        fn parseVariable(self: *Self, errorMessage: []const u8) u8 {
            self.consume(TT.IDENTIFIER, errorMessage);
            return self.identifierConstant(&self.previous);
        }

        fn defineVariable(self: *Self, global: u8) void {
            self.emitBytes(@enumToInt(OpCode.DEFINE_GLOBAL), global);
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
            _rules[@enumToInt(TT.LEFT_PAREN)] = .{ .prefix = &grouping, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RIGHT_PAREN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.LEFT_BRACE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RIGHT_BRACE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.COMMA)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.DOT)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
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
            _rules[@enumToInt(TT.AND)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.CLASS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ELSE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FALSE)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FOR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FUN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.IF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.NIL)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.OR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.PRINT)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RETURN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.SUPER)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.THIS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.TRUE)] = .{ .prefix = &literal, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.VAR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.WHILE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ERROR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EOF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            return _rules[0..];
        }
    };
}
