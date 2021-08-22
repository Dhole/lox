const expr = @import("expr.zig");
const token = @import("token.zig");

const Expr = expr.Expr;
const Token = token.Token;

pub const Var = struct {
    name: Token,
    // name: []const u8,
    initializer: ?*Expr,
};

pub const Stmt = union(enum) {
    expression: *Expr,
    print: *Expr,
    varDecl: Var,
};
