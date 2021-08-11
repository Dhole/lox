const token = @import("token.zig");

const Token = token.Token;
const Lit = token.Token.Literal;
const TT = token.TokenType;

pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

pub const Grouping = struct {
    expression: *const Expr,
};

pub const ValueTag = enum {
    boolean,
    number,
    string,
    nil,
};

pub const Value = union(ValueTag) {
    const Self = @This();

    boolean: bool,
    number: f64,
    string: []const u8,
    nil,

    pub fn asBoolean(self: *const Self) !bool {
        return switch (self.*) {
            .boolean => |v| v,
            else => error.InvalidType,
        };
    }
    pub fn asNumber(self: *const Self) !f64 {
        return switch (self.*) {
            .number => |v| v,
            else => error.InvalidType,
        };
    }
    pub fn asString(self: *const Self) ![]const u8 {
        return switch (self.*) {
            .string => |v| v,
            else => error.InvalidType,
        };
    }
    pub fn asNil(self: *const Self) !void {
        return switch (self.*) {
            .nil => void,
            else => error.InvalidType,
        };
    }
};

pub const Literal = struct {
    // This is Object in the reference implementation
    // TODO: Replace by an enum once we know all possible types
    value: Value,
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,
};

fn parenthesize(w: anytype, name: []const u8, exps: []const *const Expr) anyerror!void {
    try w.print("({s}", .{name});
    for (exps) |exp| {
        try w.print(" ", .{});
        try printAst(w, exp);
    }
    try w.print(")", .{});
}

pub fn printAst(w: anytype, exp: *const Expr) !void {
    switch (exp.*) {
        .binary => |*e| {
            try parenthesize(w, e.operator.lexeme, &.{ e.left, e.right });
        },
        .grouping => |*e| try parenthesize(w, "group", &.{e.expression}),
        .literal => |*e| switch (e.value) {
            .boolean => |v| try w.print("{s}", .{v}),
            .number => |v| try w.print("{d}", .{v}),
            .string => |v| try w.print("\"{s}\"", .{v}),
            .nil => try w.print("nil", .{}),
        },
        .unary => |*e| try parenthesize(w, e.operator.lexeme, &.{e.right}),
    }
}

const std = @import("std");

test "expr" {
    var e = Expr{ .binary = .{
        .left = &Expr{ .unary = .{
            .operator = Token.init(TT.MINUS, "-", null, 1),
            .right = &Expr{ .literal = .{ .value = .{ .number = 123.0 } } },
        } },
        .operator = Token.init(TT.STAR, "*", null, 1),
        .right = &Expr{ .grouping = .{
            .expression = &Expr{ .literal = .{ .value = .{ .number = 45.67 } } },
        } },
    } };
    var w = std.io.getStdErr().writer();
    try printAst(w, &e);
    try w.print("\n", .{});
}
