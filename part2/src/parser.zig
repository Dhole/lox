const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const expr = @import("expr.zig");
const helpers = @import("helpers.zig");

const Token = token.Token;
const TT = token.TokenType;
const Binary = expr.Binary;
const Grouping = expr.Grouping;
const Literal = expr.Literal;
const Unary = expr.Unary;
const Expr = expr.Expr;

pub const Parser = struct {
    const Self = @This();
    const Error = error{ ParseError, OutOfMemory };

    arena: std.heap.ArenaAllocator,
    tokens: ArrayList(Token),
    current: u32,

    pub fn init(allocator: *Allocator, tokens: ArrayList(Token)) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .tokens = tokens,
            .current = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Self) ?*Expr {
        return self.expression() catch |e| {
            switch (e) {
                error.ParseError => return null,
                else => unreachable,
            }
        };
    }

    fn expression(self: *Self) !*Expr {
        return self.equality();
    }

    fn binLeftAssoc(self: *Self, operandFn: fn (*Self) Error!*Expr, tokens: []const TT) !*Expr {
        var exp = try operandFn(self);

        while (self.match(tokens)) {
            var operator = self.previous();
            var right = try operandFn(self);
            var exp1 = try self.arena.allocator.create(Expr);
            exp1.* = .{ .binary = .{ .left = exp, .operator = operator, .right = right } };
            exp = exp1;
        }

        return exp;
    }

    fn equality(self: *Self) !*Expr {
        return self.binLeftAssoc(comparison, &.{ TT.BANG_EQUAL, TT.EQUAL_EQUAL });
    }

    fn comparison(self: *Self) !*Expr {
        return self.binLeftAssoc(term, &.{ TT.GREATER, TT.GREATER_EQUAL, TT.LESS, TT.LESS_EQUAL });
    }

    fn term(self: *Self) !*Expr {
        return self.binLeftAssoc(factor, &.{ TT.MINUS, TT.PLUS });
    }

    fn factor(self: *Self) !*Expr {
        return self.binLeftAssoc(unary, &.{ TT.SLASH, TT.STAR });
    }

    fn unary(self: *Self) Error!*Expr {
        if (self.match(&.{ TT.BANG, TT.MINUS })) {
            var operator = self.previous();
            var right = try self.unary();
            var e = try self.arena.allocator.create(Expr);
            e.* = .{ .unary = .{ .operator = operator, .right = right } };
            return e;
        }
        return self.primary();
    }

    fn primary(self: *Self) Error!*Expr {
        var exp = try self.arena.allocator.create(Expr);
        if (self.match(&.{TT.FALSE})) {
            exp.* = .{ .literal = .{ .value = .{ .boolean = false } } };
            return exp;
        }
        if (self.match(&.{TT.TRUE})) {
            exp.* = .{ .literal = .{ .value = .{ .boolean = true } } };
            return exp;
        }
        if (self.match(&.{TT.NIL})) {
            exp.* = .{ .literal = .{ .value = .nil } };
            return exp;
        }
        if (self.match(&.{TT.NUMBER})) {
            if (self.previous().literal) |lit| {
                switch (lit) {
                    .number => |n| {
                        exp.* = .{ .literal = .{ .value = .{ .number = n } } };
                        return exp;
                    },
                    else => unreachable,
                }
            } else {
                unreachable;
            }
        }
        if (self.match(&.{TT.STRING})) {
            if (self.previous().literal) |lit| {
                switch (lit) {
                    .string => |s| {
                        exp.* = .{ .literal = .{ .value = .{ .string = s } } };
                        return exp;
                    },
                    else => unreachable,
                }
            } else {
                unreachable;
            }
        }
        if (self.match(&.{TT.LEFT_PAREN})) {
            var exp0 = try self.expression();
            _ = try self.consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
            exp.* = .{ .grouping = .{ .expression = exp0 } };
            return exp;
        }

        return err(self.peek(), "Expect expression.");
    }

    fn match(self: *Self, types: []const TT) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Self, t: TT) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self.peek().type == t;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().type == TT.EOF;
    }

    fn peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn consume(self: *Self, typ: TT, message: []const u8) error{ParseError}!Token {
        if (self.check(typ)) {
            return self.advance();
        }
        return err(self.peek(), message);
    }

    fn err(t: Token, message: []const u8) error{ParseError} {
        helpers.printTokenErr(t, message);
        return error.ParseError;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type == TT.SEMICOLON) {
                return;
            }

            switch (self.peek().type) {
                TT.CLASS => return,
                TT.FUN => return,
                TT.VAR => return,
                TT.FOR => return,
                TT.IF => return,
                TT.WHILE => return,
                TT.PRINT => return,
                TT.RETURN => return,
                else => {},
            }

            _ = advance();
        }
    }
};

test "parser" {
    const scanner = @import("scanner.zig");
    const Scanner = scanner.Scanner;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var s = try Scanner.init(&gpa.allocator, "(1 + 2 * 3) <= 4 * 5 - 6");
    defer s.deinit();
    var tokens = try s.scanTokens();
    var parser = Parser.init(&gpa.allocator, tokens);
    defer parser.deinit();
    var exp: *Expr = undefined;
    if (parser.parse()) |e| {
        exp = e;
    } else {
        unreachable;
    }

    var w = std.io.getStdErr().writer();
    try expr.printAst(w, exp);
}
