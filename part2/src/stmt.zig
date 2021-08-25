const std = @import("std");

const ArrayList = std.ArrayList;

const expr = @import("expr.zig");
const token = @import("token.zig");

const Expr = expr.Expr;
const Token = token.Token;

pub const Var = struct {
    name: Token,
    initializer: ?*Expr,
};

pub const IfStmt = struct {
    condition: *Expr,
    thenBranch: *Stmt,
    elseBranch: ?*Stmt,
};

pub const WhileStmt = struct {
    condition: *Expr,
    body: *Stmt,
};

pub const Stmt = union(enum) {
    block: ArrayList(Stmt),
    expression: *Expr,
    ifStmt: IfStmt,
    print: *Expr,
    varDecl: Var,
    whileStmt: WhileStmt,
};
