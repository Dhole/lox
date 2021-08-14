const expr = @import("expr.zig");

const Expr = expr.Expr;

pub const Stmt = union(enum) {
    expression: *Expr,
    print: *Expr,
};
