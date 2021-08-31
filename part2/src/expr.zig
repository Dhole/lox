const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
// const interpreter = @import("interpreter.zig");

const Token = token.Token;
const Lit = token.Token.Literal;
const TT = token.TokenType;
// const Interpreter = interpreter.Interpreter;

pub const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

pub const Grouping = struct {
    expression: *const Expr,
};

pub const ValueTag = enum {
    loxFunc,
    // nativeFunc,
    boolean,
    number,
    string,
    nil,
};

// pub const NativeFunc = struct {
//     name: []const u8,
//     arity: u32,
//     call: fn (interpreter: *Interpreter, arguments: []Value, result: *Value) void,
// };

pub const LoxFunc = struct {
    declaration: Function,
};

pub const Value = union(ValueTag) {
    loxFunc: LoxFunc,
    // nativeFunc: NativeFunc,
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

pub const Logical = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

pub const Unary = struct {
    operator: Token,
    right: *const Expr,
};

pub const Variable = struct {
    name: Token,
};

pub const Assign = struct {
    name: Token,
    value: *const Expr,
};

pub const Call = struct {
    callee: *const Expr,
    paren: Token,
    arguments: ArrayList(Expr),
};

pub const Expr = union(enum) {
    binary: Binary,
    call: Call,
    grouping: Grouping,
    literal: Literal,
    logical: Logical,
    unary: Unary,
    variable: Variable,
    assign: Assign,
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
        .logical => |*e| {
            try parenthesize(w, e.operator.lexeme, &.{ e.left, e.right });
        },
        .grouping => |*e| try parenthesize(w, "group", &.{e.expression}),
        .literal => |*e| switch (e.value) {
            .loxFunc => |v| try w.print("<fun {s}>", .{v.declaration.name.lexeme}),
            .boolean => |v| try w.print("{s}", .{v}),
            .number => |v| try w.print("{d}", .{v}),
            .string => |v| try w.print("\"{s}\"", .{v}),
            .nil => try w.print("nil", .{}),
        },
        .call => |*e| try w.print("call {s}{s}", .{ e.callee, e.arguments }),
        .unary => |*e| try parenthesize(w, e.operator.lexeme, &.{e.right}),
        .variable => |*e| try w.print("var {s}", .{e.name}),
        .assign => @panic("TODO"),
    }
}

// stmt

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

pub const Function = struct {
    name: Token,
    params: ArrayList(Token),
    body: ArrayList(Stmt),
};

pub const Stmt = union(enum) {
    block: ArrayList(Stmt),
    expression: *Expr,
    function: Function,
    ifStmt: IfStmt,
    print: *Expr,
    varDecl: Var,
    whileStmt: WhileStmt,
};

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
