const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bufPrint = std.fmt.bufPrint;

const expr = @import("expr.zig");
const token = @import("token.zig");
const helpers = @import("helpers.zig");
const environment = @import("environment.zig");

const RuntimeError = helpers.RuntimeError;
const reportRuntimeError = helpers.reportRuntimeError;
const Context = helpers.Context;
const Token = token.Token;
const TT = token.TokenType;
const Literal = expr.Literal;
const Expr = expr.Expr;
const Value = expr.Value;
const ValueTag = expr.ValueTag;
const Unary = expr.Unary;
const Variable = expr.Variable;
const Assign = expr.Assign;
const Binary = expr.Binary;
const Call = expr.Call;
const Logical = expr.Logical;
const Stmt = expr.Stmt;
const Environment = environment.Environment;

const Error = error{ RuntimeError, OutOfMemory } || std.os.WriteError || std.fmt.BufPrintError;

fn fnClock(interpreter: *Interpreter, arguments: []Value, result: *Value) void {
    _ = interpreter;
    _ = arguments;
    result.* = Value{ .number = @intToFloat(f64, std.time.milliTimestamp()) / 1000.0 };
}

pub const Interpreter = struct {
    const Self = @This();

    w: anytype,
    allocator: *Allocator,
    globals: *Environment,
    env: *Environment,

    pub fn init(allocator: *Allocator) !Self {
        var env = (try allocator.create(Environment));
        env.* = Environment.init(allocator, null);
        // try env.define("clock", Value{ .nativeFunc = .{ .name = "clock", .arity = 0, .call = fnClock } });
        return Self{
            .allocator = allocator,
            .globals = env,
            .env = env,
        };
    }

    pub fn deinit(self: *Self) void {
        self.env.deinit();
        self.allocator.destroy(self.env);
        return;
    }

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
            // .nativeFunc => |va| std.mem.eql(u8, va.name, b.nativeFunc.name),
            .boolean => |va| va == b.boolean,
            .number => |va| va == b.number,
            .string => |va| std.mem.eql(u8, va, b.string),
            .nil => true,
        };
    }

    fn checkNumberOperand(c: *Context, operator: Token, operand: Value) Error!void {
        if (operand == ValueTag.number) {
            return;
        }
        try c.errSet(operator, "Operand must be a number.", .{});
        return error.RuntimeError;
    }

    fn checkNumberOperands(c: *Context, operator: Token, left: Value, right: Value) Error!void {
        if (left == ValueTag.number and right == ValueTag.number) {
            return;
        }
        try c.errSet(operator, "Operands must be numbers.", .{});
        return error.RuntimeError;
    }

    fn evalUnary(self: *Self, c: *Context, exp: *const Unary) Error!Value {
        const right = try self.eval(c, exp.right);

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

    fn evalVar(self: *Self, c: *Context, exp: *const Variable) Error!Value {
        return self.env.get(c, exp.name);
    }

    fn evalAssign(self: *Self, c: *Context, exp: *const Assign) Error!Value {
        const val = try self.eval(c, exp.value);
        try self.env.assign(c, exp.name, val);
        return val;
    }

    fn evalLogical(self: *Self, c: *Context, exp: *const Logical) Error!Value {
        const left = try self.eval(c, exp.left);

        if (exp.operator.type == TT.OR) {
            if (isTruthy(left)) {
                return left;
            }
        } else {
            if (!isTruthy(left)) {
                return left;
            }
        }
        return try self.eval(c, exp.right);
    }

    fn evalBinary(self: *Self, c: *Context, exp: *const Binary) Error!Value {
        const left = try self.eval(c, exp.left);
        const right = try self.eval(c, exp.right);

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
                try c.errSet(exp.operator, "Operands must be two numbers or two strings.", .{});
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

    fn evalCall(self: *Self, c: *Context, exp: *const Call) Error!Value {
        var callee = try self.eval(c, exp.callee);
        var arguments = ArrayList(Value).init(self.allocator);
        defer arguments.deinit();
        for (exp.arguments.items) |*arg| {
            try arguments.append(try self.eval(c, arg));
        }

        return self.call(c, exp.paren, callee, arguments.items);
    }

    fn funcArity(callee: Value) u32 {
        return switch (callee) {
            // .nativeFunc => |*n| n.arity,
            else => 0,
        };
    }

    fn loxCall(self: *Self, c: *Context, w: anytype, func: *LoxFunc, arguments: []Value, result: *Value) void {
        _ = result;
        var env = Environment.init(self.allocator, self.globals);
        for (func.declaration.items) |*param| {
            try env.define(param.lexeme, arguments[i]);
        }
        self.executeBlock(func.declaration.body);
        try self.execBlock(c, w, func.declaration.body, &env);
        return null;
    }

    fn call(self: *Self, c: *Context, w: anytype, tok: Token, callee: Value, arguments: []Value) Error!Value {
        _ = c;
        const arity = funcArity(callee);
        if (arguments.len != arity) {
            try c.errSet(tok, "Expected {d} arguments but got {d}.", .{ arity, arguments.len });
            return error.RuntimeError;
        }
        var result: Value = undefined;
        switch (callee) {
            .loxFunc => |*f| {
                self.loxCall(c, f, w, arguments, &result);
            },
            // .nativeFunc => |*f| {
            //     f.call(self, arguments, &result);
            // },
            else => {
                try c.errSet(tok, "Can only call functions and classes.", .{});
                return error.RuntimeError;
            },
        }
        return result;
    }

    pub fn eval(self: *Self, c: *Context, w: anytype, exp: *const Expr) Error!Value {
        return switch (exp.*) {
            .binary => |*e| try self.evalBinary(c, e),
            .call => |*e| try self.evalCall(c, w, e),
            .logical => |*e| try self.evalLogical(c, e),
            .grouping => |*e| try self.eval(c, w, e.expression),
            .literal => |*e| e.value,
            .unary => |*e| try self.evalUnary(c, e),
            .variable => |*e| try self.evalVar(c, e),
            .assign => |*e| try self.evalAssign(c, e),
        };
    }

    fn execBlock(self: *Self, c: *Context, w: anytype, statements: ArrayList(Stmt), env: *Environment) !void {
        var previous = self.env;
        self.env = env;
        defer {
            self.env = previous;
        }
        for (statements.items) |*stm| {
            try self.exec(c, w, stm);
        }
    }

    pub fn exec(self: *Self, c: *Context, w: anytype, stm: *const Stmt) Error!void {
        switch (stm.*) {
            .expression => |e| {
                _ = try self.eval(c, w, e);
            },
            .print => |e| {
                const val = try self.eval(c, w, e);
                try stringify(w, val);
                try w.print("\n", .{});
            },
            .varDecl => |*v| {
                var val: Value = Value{ .nil = {} };
                if (v.initializer) |ini| {
                    val = try self.eval(c, w, ini);
                }

                try self.env.define(v.name.lexeme, val);
            },
            .block => |stms| {
                var env = Environment.init(self.allocator, self.env);
                defer env.deinit();
                try self.execBlock(c, w, stms, &env);
            },
            .ifStmt => |ifStmt| {
                if (isTruthy(try self.eval(c, w, ifStmt.condition))) {
                    try self.exec(c, w, ifStmt.thenBranch);
                } else {
                    if (ifStmt.elseBranch) |elseBranch| {
                        try self.exec(c, w, elseBranch);
                    }
                }
            },
            .whileStmt => |whileStmt| {
                while (isTruthy(try self.eval(c, w, whileStmt.condition))) {
                    try self.exec(c, w, whileStmt.body);
                }
            },
        }
    }

    pub fn stringify(w: anytype, val: Value) Error!void {
        switch (val) {
            // .nativeFunc => try w.print("<native fn>", .{}),
            .boolean => |v| try w.print("{s}", .{v}),
            .number => |v| {
                try w.print("{d}", .{v});
            },
            .string => |v| try w.print("{s}", .{v}),
            .nil => try w.print("nil", .{}),
        }
    }

    pub fn interpret(self: *Self, w: anytype, stms: []const Stmt) !void {
        var ctx: Context = Context.init();
        for (stms) |*stm| {
            self.exec(&ctx, w, stm) catch {
                reportRuntimeError(ctx.err.?);
                return;
            };
        }
    }
};

test "interpreter" {
    const scanner = @import("scanner.zig");
    const parser = @import("parser.zig");

    const Scanner = scanner.Scanner;
    const Parser = parser.Parser;

    const src = "var a = 1; var b = 2; print a + b; { var a = 5; print a; } print a; print clock();";
    // const src = "var a = \"foo\"; { var a = \"bar\"; } ";
    var s = try Scanner.init(std.testing.allocator, src);
    defer s.deinit();
    var tokens = try s.scanTokens();
    var p = Parser.init(std.testing.allocator, tokens);
    defer p.deinit();
    var statements = try p.parse();
    std.debug.print("{s}\n", .{statements});

    // var int = try Interpreter.init(std.testing.allocator);
    // defer int.deinit();
    // var w = std.io.getStdErr().writer();
    // try int.interpret(w, statements.items);

    // for (statements.items) |*stm| {
    //     exec(&ctx, w, stm) catch |e| {
    //         std.debug.panic("err: {s}, ctx: {s}", .{ e, ctx });
    //     };
    // }
    // var val = exec(&ctx, w, exp) catch |e| {
    //     std.debug.panic("err: {s}, ctx: {s}", .{ e, ctx });
    // };
    // std.debug.print("{s}\n", .{val});
    // var w = std.io.getStdErr().writer();
    // try stringify(w, val);
    // std.debug.print("{d}\n", .{@field(val, "boolean")});
    // std.debug.print("{d}\n", .{val.number});
}
