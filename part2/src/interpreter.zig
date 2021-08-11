const std = @import("std");

const expr = @import("expr.zig");
const token = @import("token.zig");

const TT = token.TokenType;
const Literal = expr.Literal;
const Expr = expr.Expr;
const Value = expr.Value;
const ValueTag = expr.ValueTag;
const Unary = expr.Unary;
const Binary = expr.Binary;

const Error = error{InvalidType};

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
        .boolean => |va| va == b.asBoolean() catch unreachable,
        .number => |va| va == b.asNumber() catch unreachable,
        .string => |va| std.mem.eql(u8, va, b.asString() catch unreachable),
        .nil => true,
    };
}

fn evalUnary(exp: *const Unary) Error!Value {
    const right = try eval(exp.right);

    return switch (exp.operator.type) {
        TT.MINUS => .{ .number = -(try right.asNumber()) },
        TT.BANG => .{ .boolean = !isTruthy(right) },
        else => unreachable,
    };
}

fn evalBinary(exp: *const Binary) Error!Value {
    const left = try eval(exp.left);
    const right = try eval(exp.right);

    return switch (exp.operator.type) {
        TT.MINUS => .{ .number = (try left.asNumber()) - (try right.asNumber()) },
        TT.SLASH => .{ .number = (try left.asNumber()) / (try right.asNumber()) },
        TT.STAR => .{ .number = (try left.asNumber()) * (try right.asNumber()) },
        TT.PLUS => blk: {
            if (left == Value.number and right == Value.number) {
                break :blk .{ .number = (try left.asNumber()) + (try right.asNumber()) };
            }
            if (left == Value.string and right == Value.string) {
                @panic("'string + string' not yet implemented");
            }
            break :blk error.InvalidType;
        },
        TT.GREATER => .{ .boolean = (try left.asNumber()) > (try right.asNumber()) },
        TT.GREATER_EQUAL => .{ .boolean = (try left.asNumber()) >= (try right.asNumber()) },
        TT.LESS => .{ .boolean = (try left.asNumber()) < (try right.asNumber()) },
        TT.LESS_EQUAL => .{ .boolean = (try left.asNumber()) <= (try right.asNumber()) },
        TT.BANG_EQUAL => .{ .boolean = !isEqual(left, right) },
        TT.EQUAL_EQUAL => .{ .boolean = isEqual(left, right) },
        else => unreachable,
    };
}

pub fn eval(exp: *const Expr) Error!Value {
    return switch (exp.*) {
        .binary => |*e| try evalBinary(e),
        .grouping => |*e| try eval(e.expression),
        .literal => |*e| e.value,
        .unary => |*e| try evalUnary(e),
    };
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
    var exp: *Expr = undefined;
    if (p.parse()) |e| {
        exp = e;
    } else {
        unreachable;
    }

    var lit = eval(exp);
    std.debug.print("{s}\n", .{lit});
}
