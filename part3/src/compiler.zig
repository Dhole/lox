const std = @import("std");

const _scanner = @import("scanner.zig");
const _chunk = @import("chunk.zig");
const _token = @import("token.zig");
const _value = @import("value.zig");
const _common = @import("common.zig");
const debug = @import("debug.zig");

const print = std.debug.print;

const OpCode = _common.OpCode;
const Value = _value.Value;
const Chunk = _chunk.Chunk;
const Scanner = _scanner.Scanner;
const Token = _token.Token;
const TT = _token.TokenType;
const Flags = _common.Flags;

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

        const ParseFn = fn (*Parser(flags)) void;

        const ParseRule = struct {
            prefix: ?*const ParseFn,
            infix: ?*const ParseFn,
            precedence: Precedence,
        };

        current: Token,
        previous: Token,
        compilingChunk: *Chunk,
        scanner: *Scanner,
        hadError: bool,
        panicMode: bool,

        pub fn init() Self {
            return Self{
                .current = undefined,
                .previous = undefined,
                .compilingChunk = undefined,
                .scanner = undefined,
                .hadError = false,
                .panicMode = false,
            };
        }

        pub fn compile(self: *Self, source: []const u8, chunk: *Chunk) bool {
            self.compilingChunk = chunk;
            self.hadError = false;
            self.panicMode = false;
            self.scanner = &Scanner.init(source);

            self.advance();
            self.expression();
            self.consume(TT.EOF, "Expect end of expression.");
            self.endCompiler();
            return !self.hadError;
        }

        fn expression(self: *Self) void {
            self.parsePrecedence(Precedence.ASSIGNMENT);
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

        fn binary(self: *Self) void {
            const operatorType = self.previous.type;
            const rule = getRule(operatorType);
            self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

            switch (operatorType) {
                TT.PLUS => self.emitByte(@enumToInt(OpCode.ADD)),
                TT.MINUS => self.emitByte(@enumToInt(OpCode.SUBTRACT)),
                TT.STAR => self.emitByte(@enumToInt(OpCode.MULTIPLY)),
                TT.SLASH => self.emitByte(@enumToInt(OpCode.DIVIDE)),
                else => unreachable, // Unreachable.
            }
        }

        fn grouping(self: *Self) void {
            self.expression();
            self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
        }

        fn number(self: *Self) void {
            const value = std.fmt.parseFloat(f64, self.previous.value) catch |e| {
                std.debug.panic("{}", .{e});
            };
            _ = self.emitConstant(.{ .number = value });
        }

        fn unary(self: *Self) void {
            const operatorType = self.previous.type;
            // Compile the operand.
            self.parsePrecedence(Precedence.UNARY);

            // Emit the operator instruction.
            switch (operatorType) {
                TT.MINUS => self.emitByte(@enumToInt(OpCode.NEGATE)),
                else => unreachable, // Unreachable.
            }
        }

        fn parsePrecedence(self: *Self, precedence: Precedence) void {
            self.advance();
            const prefixRule = getRule(self.previous.type).prefix;
            if (prefixRule) |rule| {
                rule.*(self);
            } else {
                self.err("Expect expression.");
                return;
            }

            while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.type).precedence)) {
                self.advance();
                const infixRule = getRule(self.previous.type).infix;
                if (infixRule) |rule| {
                    rule.*(self);
                } else {
                    unreachable;
                }
            }
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
            _rules[@enumToInt(TT.BANG)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.BANG_EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EQUAL_EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.GREATER)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.GREATER_EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.LESS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.LESS_EQUAL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.IDENTIFIER)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.STRING)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.NUMBER)] = .{ .prefix = &number, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.AND)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.CLASS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ELSE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FALSE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FOR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.FUN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.IF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.NIL)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.OR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.PRINT)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.RETURN)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.SUPER)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.THIS)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.TRUE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.VAR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.WHILE)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.ERROR)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            _rules[@enumToInt(TT.EOF)] = .{ .prefix = null, .infix = null, .precedence = Precedence.NONE };
            return _rules[0..];
        }
    };
}
