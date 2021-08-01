const token = @import("token.zig");

const Token = token.Token;
const Lit = token.Token.Literal;
const TT = token.TokenType;

pub const Binary = struct {
    left: *Expr,
    operator: Token,
    right: *Expr,
};

pub const Grouping = struct {
    expression: *Expr,
};

pub const Literal = struct {
    // This is Object in the reference implementation
    // TODO: Replace by an enum once we know all possible types
    value: Lit,
};

pub const Unary = struct {
    operator: Token,
    right: *Expr,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,
};

fn parenthesize(w: anytype, name: []const u8, exprs: []const *Expr) anyerror!void {
    try w.print("({s}", .{name});
    for (exprs) |expr| {
        try w.print(" ", .{});
        try printAst(w, expr);
    }
    try w.print(")", .{});
}

pub fn printAst(w: anytype, expr: *Expr) !void {
    switch (expr.*) {
        .binary => |*e| {
            try parenthesize(w, e.operator.lexeme, &([_]*Expr{ e.left, e.right }));
        },
        .grouping => |*e| try parenthesize(w, "group", &([_]*Expr{e.expression})),
        .literal => |*e| try w.print("{s}", .{e.value}),
        .unary => |*e| try parenthesize(w, e.operator.lexeme, &([_]*Expr{e.right})),
    }
}

const std = @import("std");

test "expr" {
    var a: f64 = 123.0;
    var b: f64 = 45.67;
    var e = Expr{ .binary = Binary{
        .left = &Expr{ .unary = Unary{
            .operator = Token.init(TT.MINUS, "-", null, 1),
            .right = &Expr{ .literal = Literal{ .value = Lit{ .number = a } } },
        } },
        .operator = Token.init(TT.STAR, "*", null, 1),
        .right = &Expr{ .grouping = Grouping{
            .expression = &Expr{ .literal = Literal{ .value = Lit{ .number = b } } },
        } },
    } };
    var w = std.io.getStdErr().writer();
    try printAst(w, &e);
    try w.print("\n", .{});
}
