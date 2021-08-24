const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const expr = @import("expr.zig");
const stmt = @import("stmt.zig");
const helpers = @import("helpers.zig");

const Token = token.Token;
const TT = token.TokenType;
const Binary = expr.Binary;
const Grouping = expr.Grouping;
const Literal = expr.Literal;
const Unary = expr.Unary;
const Expr = expr.Expr;
const Stmt = stmt.Stmt;
const IfStmt = stmt.IfStmt;
const Var = stmt.Var;

pub const Parser = struct {
    const Self = @This();
    const Error = error{ ParseError, OutOfMemory };

    allocator: *Allocator,
    arena: std.heap.ArenaAllocator,
    tokens: ArrayList(Token),
    current: u32,

    pub fn init(allocator: *Allocator, tokens: ArrayList(Token)) Self {
        return Self{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .tokens = tokens,
            .current = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Self) !ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(&self.arena.allocator);
        while (!self.isAtEnd()) {
            try statements.append(try self.declaration());
        }
        return statements;
    }

    fn declaration(self: *Self) !Stmt {
        return blk: {
            if (self.match(&.{TT.VAR})) {
                break :blk self.varDeclaration();
            }
            break :blk self.statement();
        } catch |e| {
            switch (e) {
                error.ParseError => {
                    self.synchronize();
                    return e;
                },
                else => return e,
            }
        };
    }

    fn varDeclaration(self: *Self) !Stmt {
        const name = try self.consume(TT.IDENTIFIER, "Expect variable name.");

        var initializer: ?*Expr = null;
        if (self.match(&.{TT.EQUAL})) {
            initializer = try self.expression();
        }

        _ = try self.consume(TT.SEMICOLON, "Expect ';' after variable declaration.");
        return Stmt{ .varDecl = Var{ .name = name, .initializer = initializer } };
    }

    //  private Stmt varDeclaration() {
    //    Token name = consume(IDENTIFIER, "Expect variable name.");
    //
    //    Expr initializer = null;
    //    if (match(EQUAL)) {
    //      initializer = expression();
    //    }
    //
    //    consume(SEMICOLON, "Expect ';' after variable declaration.");
    //    return new Stmt.Var(name, initializer);
    //  }

    fn statement(self: *Self) Error!Stmt {
        if (self.match(&.{TT.IF})) {
            return self.ifStatement();
        }
        if (self.match(&.{TT.PRINT})) {
            return self.printStatement();
        }
        if (self.match(&.{TT.LEFT_BRACE})) {
            return Stmt{ .block = try self.block() };
        }
        return self.expressionStatement();
    }

    fn ifStatement(self: *Self) !Stmt {
        _ = try self.consume(TT.LEFT_PAREN, "Expect '(' after 'if'.");
        var condition = try self.expression();
        _ = try self.consume(TT.RIGHT_PAREN, "Expect ')' after if condition.");

        var thenBranch = try self.arena.allocator.create(Stmt);
        thenBranch.* = try self.statement();
        var elseBranch: ?*Stmt = null;
        if (self.match(&.{TT.ELSE})) {
            var s = try self.arena.allocator.create(Stmt);
            s.* = try self.statement();
            elseBranch = s;
        }

        return Stmt{ .ifStmt = IfStmt{ .condition = condition, .thenBranch = thenBranch, .elseBranch = elseBranch } };
    }

    fn printStatement(self: *Self) !Stmt {
        var value = try self.expression();
        _ = try self.consume(TT.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .print = value };
    }

    fn expressionStatement(self: *Self) !Stmt {
        var value = try self.expression();
        _ = try self.consume(TT.SEMICOLON, "Expect ';' after expression.");
        return Stmt{ .expression = value };
    }

    fn block(self: *Self) Error!ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(&self.arena.allocator);

        while (!self.check(TT.RIGHT_BRACE) and !self.isAtEnd()) {
            try statements.append(try self.declaration());
        }

        _ = try self.consume(TT.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    fn expression(self: *Self) !*Expr {
        return self.assignment();
    }

    fn assignment(self: *Self) Error!*Expr {
        var exp = try self.orExpr();

        if (self.match(&.{TT.EQUAL})) {
            const equals = self.previous();
            const value = try self.assignment();
            switch (exp.*) {
                .variable => |v| {
                    const name = v.name;
                    var exp1 = try self.arena.allocator.create(Expr);
                    exp1.* = .{ .assign = .{ .name = name, .value = value } };
                    exp = exp1;
                },
                else => {
                    return err(equals, "Invalid assignment target.");
                },
            }
        }
        return exp;
    }

    fn orExpr(self: *Self) !*Expr {
        var exp = try self.andExpr();

        while (self.match(&.{TT.OR})) {
            var operator = self.previous();
            var right = try self.andExpr();
            var exp1 = try self.arena.allocator.create(Expr);
            exp1.* = .{ .logical = .{ .left = exp, .operator = operator, .right = right } };
            exp = exp1;
        }

        return exp;
    }

    fn andExpr(self: *Self) !*Expr {
        var exp = try self.equality();

        while (self.match(&.{TT.AND})) {
            var operator = self.previous();
            var right = try self.equality();
            var exp1 = try self.arena.allocator.create(Expr);
            exp1.* = .{ .logical = .{ .left = exp, .operator = operator, .right = right } };
            exp = exp1;
        }

        return exp;
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
        if (self.match(&.{TT.IDENTIFIER})) {
            exp.* = .{ .variable = .{ .name = self.previous() } };
            return exp;
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

            _ = self.advance();
        }
    }
};

test "parser" {
    const scanner = @import("scanner.zig");
    const Scanner = scanner.Scanner;

    var s = try Scanner.init(std.testing.allocator, "(1 + 2 * 3) <= 4 * 5 - 6; var foo = 1 + 2;");
    defer s.deinit();
    var tokens = try s.scanTokens();
    var parser = Parser.init(std.testing.allocator, tokens);
    defer parser.deinit();
    // var exp: *Expr = undefined;
    // if (parser.parse()) |e| {
    //     exp = e;
    // } else {
    //     unreachable;
    // }
    var statements = try parser.parse();
    std.debug.print("{s}", .{statements.items});

    // var w = std.io.getStdErr().writer();
    // try expr.printAst(w, exp);
}
