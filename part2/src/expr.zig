const std = @import("std");

const Allocator = std.mem.Allocator;

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
    boolean: bool,
    number: f64,
    string: []const u8,
    nil,

    pub fn clone(self: *const Value, allocator: *Allocator) !Value {
        return switch (self.*) {
            .string => |s| blk: {
                var cpy = try allocator.dupe(u8, s);
                break :blk Value{ .string = cpy };
            },
            else => self.*,
        };
    }

    pub fn free(self: *Value, allocator: *Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            else => {},
        }
    }
};

pub const Literal = struct {
    // This is Object in the reference implementation
    value: Value,
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
};

pub const Variable = struct {
    name: Token,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,
    variable: Variable,
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
        .variable => |*e| try w.print("var {s}", .{e.name}),
    }
}

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
