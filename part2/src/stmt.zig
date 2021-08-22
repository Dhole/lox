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

pub const Stmt = union(enum) {
    block: ArrayList(Stmt),
    expression: *Expr,
    print: *Expr,
    varDecl: Var,
};
