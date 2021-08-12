const std = @import("std");

const expr = @import("expr.zig");
const token = @import("token.zig");
const helpers = @import("helpers.zig");

const RuntimeError = helpers.RuntimeError;
const reportRuntimeError = helpers.reportRuntimeError;
const Token = token.Token;
const TT = token.TokenType;
const Literal = expr.Literal;
const Expr = expr.Expr;
const Value = expr.Value;
const ValueTag = expr.ValueTag;
const Unary = expr.Unary;
const Binary = expr.Binary;

const Error = error{RuntimeError};

pub const Context = struct {
    const Self = @This();
    err: ?RuntimeError,

    pub fn init() Self {
        return Self{
            .err = null,
        };
    }
};

fn isTruthy(val: Value) bool {
    return switch (val) {
        .nil => false,
        .boolean => |v| v,
        else => true,
    };
}

fn isEqual(a: Value, b: Value) bool {
    if (@as(ValueTag, a) != @as(ValueTag, b)) {
        return false;
    }
    return switch (a) {
        .boolean => |va| va == b.boolean,
        .number => |va| va == b.number,
        .string => |va| std.mem.eql(u8, va, b.string),
        .nil => true,
    };
}

fn checkNumberOperand(c: *Context, operator: Token, operand: Value) error{RuntimeError}!void {
    if (operand == ValueTag.number) {
        return;
    }
    c.err = .{ .tok = operator, .msg = "Operand must be a number." };
    return error.RuntimeError;
}

fn checkNumberOperands(c: *Context, operator: Token, left: Value, right: Value) error{RuntimeError}!void {
    if (left == ValueTag.number and right == ValueTag.number) {
        return;
    }
    c.err = .{ .tok = operator, .msg = "Operands must be numbers." };
    return error.RuntimeError;
}

fn evalUnary(c: *Context, exp: *const Unary) Error!Value {
    const right = try eval(c, exp.right);

    switch (exp.operator.type) {
        TT.MINUS => {
            try checkNumberOperand(c, exp.operator, right);
            return Value{ .number = -(right.number) };
        },
        TT.BANG => {
            return Value{ .boolean = !isTruthy(right) };
        },
        else => unreachable,
    }
}

fn evalBinary(c: *Context, exp: *const Binary) Error!Value {
    const left = try eval(c, exp.left);
    const right = try eval(c, exp.right);

    switch (exp.operator.type) {
        TT.MINUS => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .number = (left.number) - (right.number) };
        },
        TT.SLASH => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .number = (left.number) / (right.number) };
        },
        TT.STAR => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .number = (left.number) * (right.number) };
        },
        TT.PLUS => {
            if (left == .number and right == .number) {
                return Value{ .number = (left.number) + (right.number) };
            }
            if (left == .string and right == .string) {
                @panic("'string + string' not yet implemented");
            }
            c.err = .{ .tok = exp.operator, .msg = "Operands must be two numbers or two strings." };
            return error.RuntimeError;
        },
        TT.GREATER => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .boolean = (left.number) > (right.number) };
        },
        TT.GREATER_EQUAL => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .boolean = (left.number) >= (right.number) };
        },
        TT.LESS => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .boolean = (left.number) < (right.number) };
        },
        TT.LESS_EQUAL => {
            try checkNumberOperands(c, exp.operator, left, right);
            return Value{ .boolean = (left.number) <= (right.number) };
        },
        TT.BANG_EQUAL => {
            return Value{ .boolean = !isEqual(left, right) };
        },
        TT.EQUAL_EQUAL => {
            return Value{ .boolean = isEqual(left, right) };
        },
        else => unreachable,
    }
}

pub fn eval(c: *Context, exp: *const Expr) Error!Value {
    return switch (exp.*) {
        .binary => |*e| try evalBinary(c, e),
        .grouping => |*e| try eval(c, e.expression),
        .literal => |*e| e.value,
        .unary => |*e| try evalUnary(c, e),
    };
}

pub fn stringify(w: anytype, val: Value) !void {
    const min_i64 = std.math.minInt(i64);
    const max_i64 = std.math.maxInt(i64);
    switch (val) {
        .boolean => |v| try w.print("{s}", .{v}),
        .number => |v| {
            if (min_i64 <= v and v <= max_i64 and (v - @intToFloat(f64, @floatToInt(i64, v))) == 0.0) {
                try w.print("{d:.0}", .{v});
            } else {
                try w.print("{d}", .{v});
            }
        },
        .string => |v| try w.print("{s}", .{v}),
        .nil => try w.print("nil", .{}),
    }
}

pub fn interpret(w: anytype, exp: *const Expr) !void {
    var ctx: Context = Context.init();
    const val = eval(&ctx, exp) catch {
        reportRuntimeError(ctx.err.?);
        return;
    };
    try stringify(w, val);
    try w.print("\n", .{});
}

test "interpreter" {
    const scanner = @import("scanner.zig");
    const parser = @import("parser.zig");

    const Scanner = scanner.Scanner;
    const Parser = parser.Parser;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var s = try Scanner.init(&gpa.allocator, "(1 + 2 * 3 - 4)");
    defer s.deinit();
    var tokens = try s.scanTokens();
    var p = Parser.init(&gpa.allocator, tokens);
    defer p.deinit();
    var exp = p.parse().?;

    var ctx: Context = Context.init();
    var val = eval(&ctx, exp) catch |e| {
        std.debug.panic("err: {s}, ctx: {s}", .{ e, ctx });
    };
    std.debug.print("{s}\n", .{val});
    var w = std.io.getStdErr().writer();
    try stringify(w, val);
    // std.debug.print("{d}\n", .{@field(val, "boolean")});
    // std.debug.print("{d}\n", .{val.number});
}
