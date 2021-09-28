const std = @import("std");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;

const interpreter = @import("interpreter.zig");
const expr = @import("expr.zig");
const token = @import("token.zig");
const helpers = @import("helpers.zig");

const Token = token.Token;
const Interpreter = interpreter.Interpreter;
const Stmt = expr.Stmt;
const Expr = expr.Expr;
const Function = expr.Function;
const Context = helpers.Context;
const reportResolveError = helpers.reportResolveError;

const Error = error{ OutOfMemory, ResolveError } || std.fmt.BufPrintError;

pub const FunctionType = enum {
    NONE,
    FUNCTION,
};

pub const Resolver = struct {
    const Self = @This();

    allocator: *Allocator,
    int: *Interpreter,
    scopes: ArrayList(StringHashMap(bool)),
    currentFunc: FunctionType,

    pub fn init(int: *Interpreter, allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .int = int,
            .scopes = ArrayList(StringHashMap(bool)).init(allocator),
            .currentFunc = .NONE,
        };
    }

    pub fn deinit(self: *Self) void {
        // TODO: free each scope
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    fn beginScope(self: *Self) !void {
        try self.scopes.append(StringHashMap(bool).init(self.allocator));
    }

    fn endScope(self: *Self) void {
        var scope = self.scopes.pop();
        scope.deinit();
    }

    fn declare(self: *Self, c: anytype, name: Token) !void {
        if (self.scopesIsEmpty()) {
            return;
        }
        var scope = self.scopesPeek();
        if (scope.contains(name.lexeme)) {
            try c.errSet(name, "Already a variable with this name in this scope.", .{});
            return error.ResolveError;
        }

        try scope.put(name.lexeme, false);
    }

    fn define(self: *Self, name: Token) !void {
        if (self.scopesIsEmpty()) {
            return;
        }
        var scope = self.scopesPeek();
        try scope.put(name.lexeme, true);
    }

    fn scopesPeek(self: *Self) *StringHashMap(bool) {
        return &self.scopes.items[self.scopes.items.len - 1];
    }

    fn scopesIsEmpty(self: *Self) bool {
        return self.scopes.items.len == 0;
    }

    fn resolveLocal(self: *Self, exp: *const c_void, name: Token) !void {
        // var i = self.scopes.items.len - 1;
        // while (i >= 0) : (i -= 1) {
        //     if (self.scopes.items[i].contains(name.lexeme)) {
        //         self.int._resolve(exp, self.scopes.items.len - 1 - i);
        //         return;
        //     }
        // }
        var i: usize = 0;
        while (i < self.scopes.items.len) : (i += 1) {
            if (self.scopes.items[self.scopes.items.len - 1 - i].contains(name.lexeme)) {
                try self.int.resolve(exp, i);
                return;
            }
        }
    }

    fn resolveExpr(self: *Self, c: anytype, exp: *const Expr) Error!void {
        _ = self;
        switch (exp.*) {
            .variable => |*e| {
                if (!self.scopesIsEmpty()) {
                    if (self.scopesPeek().get(e.name.lexeme)) |defined| {
                        if (defined == false) {
                            try c.errSet(e.name, "Can't read local variable in its own initializer.", .{});
                            return error.ResolveError;
                        }
                    }
                }
                try self.resolveLocal(e, e.name);
            },
            .assign => |*e| {
                try self.resolveExpr(c, e.value);
                try self.resolveLocal(e, e.name);
            },
            .binary => |*e| {
                try self.resolveExpr(c, e.left);
                try self.resolveExpr(c, e.right);
            },
            .call => |*e| {
                try self.resolveExpr(c, e.callee);
                for (e.arguments.items) |*argument| {
                    try self.resolveExpr(c, argument);
                }
            },
            .get => |*e| {
                try self.resolveExpr(c, e.object);
            },
            .set => |*e| {
                try self.resolveExpr(c, e.value);
                try self.resolveExpr(c, e.object);
            },
            .grouping => |*e| {
                try self.resolveExpr(c, e.expression);
            },
            .literal => {},
            .logical => |*e| {
                try self.resolveExpr(c, e.left);
                try self.resolveExpr(c, e.right);
            },
            .unary => |*e| {
                try self.resolveExpr(c, e.right);
            },
        }
    }

    fn resolveFunction(self: *Self, c: anytype, f: *const Function, funcType: FunctionType) !void {
        var enclosingFunc = self.currentFunc;
        self.currentFunc = funcType;
        defer {
            self.currentFunc = enclosingFunc;
        }
        try self.beginScope();
        for (f.params.items) |param| {
            try self.declare(c, param);
            try self.define(param);
        }
        try self._resolve(c, f.body.items);
        self.endScope();
    }

    fn resolveStmt(self: *Self, c: anytype, stm: *const Stmt) Error!void {
        switch (stm.*) {
            .block => |stms| {
                try self.beginScope();
                try self._resolve(c, stms.items);
                self.endScope();
            },
            .expression => |e| {
                try self.resolveExpr(c, e);
            },
            .print => |e| {
                try self.resolveExpr(c, e);
            },
            .retStmt => |retStmt| {
                if (self.currentFunc == .NONE) {
                    try c.errSet(retStmt.keyword, "Can't return from top-level code.", .{});
                    return error.ResolveError;
                }
                try self.resolveExpr(c, retStmt.value);
            },
            .varDecl => |*v| {
                try self.declare(c, v.name);
                if (v.initializer) |ini| {
                    try self.resolveExpr(c, ini);
                }
                try self.define(v.name);
            },
            .function => |*f| {
                try self.declare(c, f.name);
                try self.define(f.name);

                try self.resolveFunction(c, f, .FUNCTION);
            },
            .class => |*s| {
                try self.declare(c, s.name);
                try self.define(s.name);
            },
            .ifStmt => |ifStmt| {
                try self.resolveExpr(c, ifStmt.condition);
                try self.resolveStmt(c, ifStmt.thenBranch);
                if (ifStmt.elseBranch) |elseBranch| {
                    try self.resolveStmt(c, elseBranch);
                }
            },
            .whileStmt => |whileStmt| {
                try self.resolveExpr(c, whileStmt.condition);
                try self.resolveStmt(c, whileStmt.body);
            },
        }
    }

    pub fn _resolve(self: *Self, c: anytype, stms: []const Stmt) Error!void {
        for (stms) |*stm| {
            try self.resolveStmt(c, stm);
        }
    }

    pub fn resolve(self: *Self, stms: []const Stmt) Error!void {
        var ctx = Context(@TypeOf(.{})).init(.{});
        self._resolve(&ctx, stms) catch |e| {
            switch (e) {
                error.ResolveError => {
                    reportResolveError(ctx.err.?);
                    return;
                },
                else => return e,
            }
        };
    }
};

test "resolver" {
    const scanner = @import("scanner.zig");
    const parser = @import("parser.zig");

    const Scanner = scanner.Scanner;
    const Parser = parser.Parser;

    const src = "var a = 1; var b = 2; print a + b; { var a = 5; print a; } print a; print clock();";

    var s = try Scanner.init(std.testing.allocator, src);
    defer s.deinit();
    var tokens = try s.scanTokens();

    var int = try Interpreter.init(std.testing.allocator);
    defer int.deinit();
    var p = try Parser.init(std.testing.allocator, &int.funcArena, tokens);
    defer p.deinit();
    var statements = try p.parse();

    var res = try Resolver.init(&int, std.testing.allocator);
    defer res.deinit();
    try res.resolve(statements.items);
}
