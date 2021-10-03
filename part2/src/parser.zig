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
const Stmt = expr.Stmt;
const Var = expr.Var;
const Variable = expr.Variable;
const Function = expr.Function;

pub const Parser = struct {
    const Self = @This();
    const Error = error{ ParseError, OutOfMemory };

    allocator: *Allocator,
    arena: *std.heap.ArenaAllocator,
    funcArena: *std.heap.ArenaAllocator,
    tokens: ArrayList(Token),
    current: u32,

    pub fn init(allocator: *Allocator, funcArena: *std.heap.ArenaAllocator, tokens: ArrayList(Token)) !Self {
        var arena = (try allocator.create(std.heap.ArenaAllocator));
        arena.* = std.heap.ArenaAllocator.init(allocator);
        return Self{
            .allocator = allocator,
            .arena = arena,
            .funcArena = funcArena,
            .tokens = tokens,
            .current = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.allocator.destroy(self.arena);
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
            if (self.match(&.{TT.CLASS})) {
                break :blk self.classDeclaration();
            } else if (self.match(&.{TT.FUN})) {
                break :blk self.function(FunctionKind.function);
            } else if (self.match(&.{TT.VAR})) {
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

        _ = try self._consume(TT.SEMICOLON, "Expect ';' after variable declaration.");
        return Stmt{ .varDecl = Var{ .name = name, .initializer = initializer } };
    }

    const FunctionKind = enum {
        function,
        method,
    };

    fn function(self: *Self, kind: FunctionKind) !Stmt {
        // Alloc function in the funcArena so that we don't free it after
        // storing it in the interpreter environment.
        const arena = self.arena;
        self.arena = self.funcArena;
        defer {
            self.arena = arena;
        }
        const name = try self.consume(TT.IDENTIFIER, switch (kind) {
            .function => "Expect function name.",
            .method => "Expect method name.",
        });
        _ = try self._consume(TT.LEFT_PAREN, switch (kind) {
            .function => "Expect '(' after function name.",
            .method => "Expect '(' after method name.",
        });

        var parameters = ArrayList(Token).init(&self.arena.allocator);
        if (!self.check(TT.RIGHT_PAREN)) {
            while (true) {
                if (parameters.items.len >= 255) {
                    return err(try self.peek(), "Can't have more than 255 parameters.");
                }
                try parameters.append(try self.consume(TT.IDENTIFIER, "Expect parameter name."));
                if (!self.match(&.{TT.COMMA})) {
                    break;
                }
            }
        }
        _ = try self._consume(TT.RIGHT_PAREN, "Expect ')' after parameters.");

        _ = try self._consume(TT.LEFT_BRACE, switch (kind) {
            .function => "Expect '{' before function body.",
            .method => "Expect '{' before method body.",
        });
        const body = try self.block();
        return Stmt{ .function = .{ .name = name, .params = parameters, .body = body } };
    }

    fn classDeclaration(self: *Self) !Stmt {
        const name = try self.consume(TT.IDENTIFIER, "Expect class name.");
        var superclass: ?Variable = null;
        if (self.match(&.{TT.LESS})) {
            const superclassName = try self.consume(TT.IDENTIFIER, "Expect superclass name.");
            superclass = Variable{ .name = superclassName };
        }
        _ = try self.consume(TT.LEFT_BRACE, "Expect '{' before class body.");
        var methods = ArrayList(Function).init(&self.arena.allocator);
        while (!self.check(TT.RIGHT_BRACE) and !self.isAtEnd()) {
            try methods.append((try self.function(FunctionKind.method)).function);
        }
        _ = try self.consume(TT.RIGHT_BRACE, "Expect '}' after class body.");
        return Stmt{ .class = .{ .name = name, .superclass = superclass, .methods = methods } };
    }

    fn statement(self: *Self) Error!Stmt {
        if (self.match(&.{TT.IF})) {
            return self.ifStatement();
        }
        if (self.match(&.{TT.PRINT})) {
            return self.printStatement();
        }
        if (self.match(&.{TT.RETURN})) {
            return self.returnStatement();
        }
        if (self.match(&.{TT.WHILE})) {
            return self.whileStatement();
        }
        if (self.match(&.{TT.FOR})) {
            return self.forStatement();
        }
        if (self.match(&.{TT.LEFT_BRACE})) {
            return Stmt{ .block = try self.block() };
        }
        return self.expressionStatement();
    }

    fn ifStatement(self: *Self) !Stmt {
        _ = try self._consume(TT.LEFT_PAREN, "Expect '(' after 'if'.");
        var condition = try self.expression();
        _ = try self._consume(TT.RIGHT_PAREN, "Expect ')' after if condition.");

        var thenBranch = try self.arena.allocator.create(Stmt);
        thenBranch.* = try self.statement();
        var elseBranch: ?*Stmt = null;
        if (self.match(&.{TT.ELSE})) {
            var s = try self.arena.allocator.create(Stmt);
            s.* = try self.statement();
            elseBranch = s;
        }

        return Stmt{ .ifStmt = .{ .condition = condition, .thenBranch = thenBranch, .elseBranch = elseBranch } };
    }

    fn whileStatement(self: *Self) !Stmt {
        _ = try self._consume(TT.LEFT_PAREN, "Expect '(' after 'while'.");
        var condition = try self.expression();
        _ = try self._consume(TT.RIGHT_PAREN, "Expect ')' after condition.");
        var body = try self.arena.allocator.create(Stmt);
        body.* = try self.statement();

        return Stmt{ .whileStmt = .{ .condition = condition, .body = body } };
    }

    fn forStatement(self: *Self) !Stmt {
        _ = try self._consume(TT.LEFT_PAREN, "Expect '(' after 'while'.");

        var initializer: ?*Stmt = undefined;
        if (self.match(&.{TT.SEMICOLON})) {
            initializer = null;
        } else if (self.match(&.{TT.VAR})) {
            var _initializer = try self.arena.allocator.create(Stmt);
            _initializer.* = try self.varDeclaration();
            initializer = _initializer;
        } else {
            var _initializer = try self.arena.allocator.create(Stmt);
            _initializer.* = try self.expressionStatement();
            initializer = _initializer;
        }

        var condition: ?*Expr = null;
        if (!self.check(TT.SEMICOLON)) {
            condition = try self.expression();
        }
        _ = try self._consume(TT.SEMICOLON, "Expect ';' after loop condition.");

        var increment: ?*Expr = null;
        if (!self.check(TT.RIGHT_PAREN)) {
            increment = try self.expression();
        }
        _ = try self._consume(TT.RIGHT_PAREN, "Expect ')' after for clauses.");

        var body = try self.statement();

        if (increment) |inc| {
            var statements = ArrayList(Stmt).init(&self.arena.allocator);
            try statements.append(body);
            try statements.append(Stmt{ .expression = inc });
            body = Stmt{ .block = statements };
        }

        var cond: *Expr = undefined;
        if (condition) |c| {
            cond = c;
        } else {
            var c = try self.arena.allocator.create(Expr);
            c.* = Expr{ .literal = .{ .value = .{ .boolean = true } } };
            cond = c;
        }
        var _body = try self.arena.allocator.create(Stmt);
        _body.* = body;
        body = Stmt{ .whileStmt = .{ .condition = cond, .body = _body } };

        if (initializer) |ini| {
            var statements = ArrayList(Stmt).init(&self.arena.allocator);
            try statements.append(ini.*);
            try statements.append(body);
            body = Stmt{ .block = statements };
        }

        return body;
    }

    fn printStatement(self: *Self) !Stmt {
        var value = try self.expression();
        _ = try self._consume(TT.SEMICOLON, "Expect ';' after value.");
        return Stmt{ .print = value };
    }

    fn returnStatement(self: *Self) !Stmt {
        var keyword = try self.previous();
        var value: *Expr = undefined;
        if (!self.check(TT.SEMICOLON)) {
            value = try self.expression();
        } else {
            value = try self.arena.allocator.create(Expr);
            value.* = Expr{ .literal = .{ .value = .nil } };
        }
        _ = try self._consume(TT.SEMICOLON, "Expect ';' after return value.");
        return Stmt{ .retStmt = .{ .keyword = keyword, .value = value } };
    }

    fn expressionStatement(self: *Self) !Stmt {
        var value = try self.expression();
        _ = try self._consume(TT.SEMICOLON, "Expect ';' after expression.");
        return Stmt{ .expression = value };
    }

    fn block(self: *Self) Error!ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(&self.arena.allocator);

        while (!self.check(TT.RIGHT_BRACE) and !self.isAtEnd()) {
            try statements.append(try self.declaration());
        }

        _ = try self._consume(TT.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    fn expression(self: *Self) !*Expr {
        return self.assignment();
    }

    fn assignment(self: *Self) Error!*Expr {
        var exp = try self.orExpr();

        if (self.match(&.{TT.EQUAL})) {
            const equals = try self.previous();
            const value = try self.assignment();
            switch (exp.*) {
                .variable => |v| {
                    const name = v.name;
                    var exp1 = try self.arena.allocator.create(Expr);
                    exp1.* = .{ .assign = .{ .name = name, .value = value } };
                    exp = exp1;
                },
                .get => |g| {
                    var exp1 = try self.arena.allocator.create(Expr);
                    exp1.* = .{ .set = .{ .name = g.name, .object = g.object, .value = value } };
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
            var operator = try self.previous();
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
            var operator = try self.previous();
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
            var operator = try self.previous();
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
            var operator = try self.previous();
            var right = try self.unary();
            var e = try self.arena.allocator.create(Expr);
            e.* = .{ .unary = .{ .operator = operator, .right = right } };
            return e;
        }
        return self.call();
    }

    fn call(self: *Self) Error!*Expr {
        var exp = try self.primary();

        while (true) {
            if (self.match(&.{TT.LEFT_PAREN})) {
                exp = try self.finishCall(exp);
            } else if (self.match(&.{TT.DOT})) {
                var name = try self.consume(TT.IDENTIFIER, "Expect property name after '.'.");
                var exp1 = try self.arena.allocator.create(Expr);
                exp1.* = Expr{ .get = .{ .name = name, .object = exp } };
                exp = exp1;
            } else {
                break;
            }
        }

        return exp;
    }

    fn finishCall(self: *Self, callee: *Expr) Error!*Expr {
        var arguments = ArrayList(Expr).init(&self.arena.allocator);
        if (!self.check(TT.RIGHT_PAREN)) {
            while (true) {
                if (arguments.items.len >= 255) {
                    return err(try self.peek(), "Can't have more than 255 arguments.");
                }
                try arguments.append((try self.expression()).*);
                if (!self.match(&.{TT.COMMA})) {
                    break;
                }
            }
        }

        var paren = try self.consume(TT.RIGHT_PAREN, "Expect ')' after arguments.");

        var exp = try self.arena.allocator.create(Expr);
        exp.* = Expr{ .call = .{ .callee = callee, .paren = paren, .arguments = arguments } };
        return exp;
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
            if ((try self.previous()).literal) |lit| {
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
            if ((try self.previous()).literal) |lit| {
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
        if (self.match(&.{TT.SUPER})) {
            var keyword = try self.previous();
            _ = try self.consume(TT.DOT, "Expect '.' after 'super'.");
            const method = try self.consume(TT.IDENTIFIER, "Expect superclass method name.");
            exp.* = .{ .super = .{ .keyword = keyword, .method = method } };
            return exp;
        }
        if (self.match(&.{TT.THIS})) {
            exp.* = .{ .this = .{ .keyword = try self.previous() } };
            return exp;
        }
        if (self.match(&.{TT.IDENTIFIER})) {
            exp.* = .{ .variable = .{ .name = try self.previous() } };
            return exp;
        }
        if (self.match(&.{TT.LEFT_PAREN})) {
            var exp0 = try self.expression();
            _ = try self._consume(TT.RIGHT_PAREN, "Expect ')' after expression.");
            exp.* = .{ .grouping = .{ .expression = exp0 } };
            return exp;
        }

        return err(try self.peek(), "Expect expression.");
    }

    fn match(self: *Self, types: []const TT) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self._advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: *Self, t: TT) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self._peek().type == t;
    }

    fn advance(self: *Self) !Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return try self.previous();
    }

    fn _advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self._previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self._peek().type == TT.EOF;
    }

    fn _peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    fn peek(self: *Self) !Token {
        const tok = self.tokens.items[self.current];
        // If we're parsing a function, return a copy of the token allocated in
        // the funcArena, so that we don't free the tokens in the Function.
        if (self.arena == self.funcArena) {
            return try tok.clone(&self.arena.allocator);
        } else {
            return tok;
        }
    }

    fn _previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn previous(self: *Self) !Token {
        const tok = self.tokens.items[self.current - 1];
        // If we're parsing a function, return a copy of the token allocated in
        // the funcArena, so that we don't free the tokens in the Function.
        if (self.arena == self.funcArena) {
            return try tok.clone(&self.arena.allocator);
        } else {
            return tok;
        }
    }

    fn _consume(self: *Self, typ: TT, message: []const u8) error{ParseError}!Token {
        if (self.check(typ)) {
            return self._advance();
        }
        return err(self._peek(), message);
    }

    fn consume(self: *Self, typ: TT, message: []const u8) error{ ParseError, OutOfMemory }!Token {
        if (self.check(typ)) {
            return self.advance();
        }
        return err(self._peek(), message);
    }

    fn err(t: Token, message: []const u8) error{ParseError} {
        helpers.printTokenErr(t, message);
        return error.ParseError;
    }

    fn synchronize(self: *Self) void {
        _ = self._advance();

        while (!self.isAtEnd()) {
            if (self._previous().type == TT.SEMICOLON) {
                return;
            }

            switch (self._peek().type) {
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

            _ = self._advance();
        }
    }
};

test "parser" {
    const scanner = @import("scanner.zig");
    const Scanner = scanner.Scanner;

    // var s = try Scanner.init(std.testing.allocator, "(1 + 2 * 3) <= 4 * 5 - 6; var foo = 1 + 2;");
    var s = try Scanner.init(std.testing.allocator, "foo(1, 2); bar(); fun foo(a) { return 1 + 2; }");
    defer s.deinit();
    var tokens = try s.scanTokens();
    var funcArena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer funcArena.deinit();
    var parser = try Parser.init(std.testing.allocator, &funcArena, tokens);
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
