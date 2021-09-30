const std = @import("std");

const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bufPrint = std.fmt.bufPrint;
const HashMap = std.HashMap;
const AutoContext = std.hash_map.AutoContext;

const expr = @import("expr.zig");
const token = @import("token.zig");
const helpers = @import("helpers.zig");
const environment = @import("environment.zig");
const resolver = @import("resolver.zig");

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
const NativeFunc = expr.NativeFunc;
const LoxFunc = expr.LoxFunc;
const LoxClass = expr.LoxClass;
const Get = expr.Get;
const Set = expr.Set;
const This = expr.This;
const LoxInstance = expr.LoxInstance;
const Environment = environment.Environment;
const Resolver = resolver.Resolver;

const Error = error{ RuntimeError, Return } || std.mem.Allocator.Error || std.os.WriteError || std.fmt.BufPrintError;

fn fnClock(interpreter: *Interpreter, arguments: []Value, result: *Value) void {
    _ = interpreter;
    _ = arguments;
    result.* = Value{ .number = @intToFloat(f64, std.time.milliTimestamp()) / 1000.0 };
}

// `c: anytype` is Context(anytype)
pub const Interpreter = struct {
    const Self = @This();

    scratch: std.heap.ArenaAllocator,
    scratchLen: u32,
    allocator: *Allocator,
    funcArena: std.heap.ArenaAllocator,
    globals: *Environment,
    env: *Environment,
    locals: HashMap(*const c_void, usize, AutoContext(*const c_void), 80),

    pub fn init(allocator: *Allocator) !Self {
        var env = (try allocator.create(Environment));
        env.* = Environment.init(allocator, null);
        try env.define("clock", Value{ .nativeFunc = .{ .arity = 0, .call = fnClock } });
        return Self{
            .allocator = allocator,
            .funcArena = std.heap.ArenaAllocator.init(allocator),
            .scratch = std.heap.ArenaAllocator.init(allocator),
            .scratchLen = 0,
            .globals = env,
            .env = env,
            .locals = HashMap(*const c_void, usize, AutoContext(*const c_void), 80).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self.globals.deinit();
        self.allocator.destroy(self.globals);
        self.funcArena.deinit();
        self.scratch.deinit();
        self.locals.deinit();
        return;
    }

    pub fn resolve(self: *Self, exp: *const c_void, n: usize) !void {
        // std.debug.print("DBG int.resolve({*}, {d})\n", .{ exp, n });
        try self.locals.put(exp, n);
    }

    fn scratchAlloc(self: *Self, comptime T: type, n: anytype) ![]T {
        var v = try self.scratch.allocator.alloc(T, n);
        self.scratchLen += @intCast(u32, n);
        return v;
    }

    fn scratchReset(self: *Self) void {
        self.scratch.deinit();
        self.scratch = std.heap.ArenaAllocator.init(self.allocator);
        self.scratchLen = 0;
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
            .loxFunc => |va| std.mem.eql(u8, va.declaration.name.lexeme, b.loxFunc.declaration.name.lexeme),
            .loxClass => |va| std.mem.eql(u8, va.name, b.loxClass.name),
            .loxInstance => false,
            .nativeFunc => |va| va.call == b.nativeFunc.call,
            .boolean => |va| va == b.boolean,
            .number => |va| va == b.number,
            .string => |va| std.mem.eql(u8, va, b.string),
            .nil => true,
        };
    }

    fn checkNumberOperand(c: anytype, operator: Token, operand: Value) Error!void {
        if (operand == ValueTag.number) {
            return;
        }
        try c.errSet(operator, "Operand must be a number.", .{});
        return error.RuntimeError;
    }

    fn checkNumberOperands(c: anytype, operator: Token, left: Value, right: Value) Error!void {
        if (left == ValueTag.number and right == ValueTag.number) {
            return;
        }
        try c.errSet(operator, "Operands must be numbers.", .{});
        return error.RuntimeError;
    }

    fn evalUnary(self: *Self, c: anytype, exp: *const Unary) Error!Value {
        var right = try self.eval(c, exp.right);
        defer right.unref();

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

    fn lookUpVariable(self: *Self, c: anytype, name: Token, exp: *const c_void) Error!Value {
        if (self.locals.get(exp)) |distance| {
            return self.env.getAt(distance, name.lexeme);
        } else {
            return self.globals.get(c, name);
        }
    }

    fn evalVar(self: *Self, c: anytype, exp: *const Variable) Error!Value {
        return self.lookUpVariable(c, exp.name, exp);
        // return self.env.get(c, exp.name);
    }

    fn evalAssign(self: *Self, c: anytype, exp: *const Assign) Error!Value {
        var val = try self.eval(c, exp.value);
        // try self.env.assign(c, exp.name, val.ref(false));
        if (self.locals.get(exp)) |distance| {
            try self.env.assignAt(distance, exp.name, val.ref(false));
        } else {
            try self.globals.assign(c, exp.name, val.ref(false));
        }
        return val;
    }

    fn evalLogical(self: *Self, c: anytype, exp: *const Logical) Error!Value {
        var left = try self.eval(c, exp.left);
        defer left.unref();

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

    fn evalBinary(self: *Self, c: anytype, exp: *const Binary) Error!Value {
        var left = try self.eval(c, exp.left);
        defer left.unref();
        var right = try self.eval(c, exp.right);
        defer right.unref();

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
                    const l = left.string;
                    const r = right.string;
                    var lr = try self.scratchAlloc(u8, l.len + r.len);
                    std.mem.copy(u8, lr[0..], l);
                    std.mem.copy(u8, lr[l.len..], r);
                    return Value{ .string = lr };
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

    fn evalCall(self: *Self, c: anytype, exp: *const Call) Error!Value {
        var callee = try self.eval(c, exp.callee);
        var arguments = ArrayList(Value).init(self.allocator);
        defer arguments.deinit();
        for (exp.arguments.items) |*arg| {
            try arguments.append(try self.eval(c, arg));
        }
        defer {
            for (arguments.items) |*val| {
                val.unref();
            }
        }

        return self.call(c, exp.paren, callee, arguments.items);
    }

    fn evalGet(self: *Self, c: anytype, exp: *const Get) Error!Value {
        var object = try self.eval(c, exp.object);
        switch (object) {
            .loxInstance => |i| {
                return i.get(c, exp.name);
            },
            else => {
                try c.errSet(exp.name, "Only instances have properties.", .{});
                return error.RuntimeError;
            },
        }
    }

    fn evalSet(self: *Self, c: anytype, exp: *const Set) Error!Value {
        var object = try self.eval(c, exp.object);
        switch (object) {
            .loxInstance => |i| {
                // return i.get(c, exp.name);
                var value = try self.eval(c, exp.value);
                try i.set(exp.name, value.ref(false));
                return value;
            },
            else => {
                try c.errSet(exp.name, "Only instances have fields.", .{});
                return error.RuntimeError;
            },
        }
    }

    fn evalThis(self: *Self, c: anytype, exp: *const This) Error!Value {
        return self.lookUpVariable(c, exp.keyword, exp);
    }

    fn funcArity(callee: Value) u32 {
        return switch (callee) {
            .loxFunc => |*f| @intCast(u32, f.declaration.params.items.len),
            .loxClass => |f| blk: {
                if (f.findMethod("init")) |initializer| {
                    break :blk funcArity(Value{ .loxFunc = initializer });
                }
                break :blk 0;
            },
            .nativeFunc => |*f| f.arity,
            else => 0,
        };
    }

    fn loxCall(self: *Self, c: anytype, func: *const LoxFunc, arguments: []Value, result: *Value) !void {
        _ = result;
        var env = try self.allocator.create(Environment);
        env.* = Environment.init(self.allocator, func.closure);
        defer {
            if (env.deinit()) {
                // std.debug.print("DBG destroy env{*}\n", .{env});
                self.allocator.destroy(env);
            }
        }
        for (func.declaration.params.items) |*param, i| {
            try env.define(param.lexeme, arguments[i]);
        }
        self.execBlock(c, func.declaration.body, env) catch |err| {
            switch (err) {
                error.Return => {
                    if (func.isInitializer) {
                        result.* = func.closure.getAt(0, "this");
                        return;
                    }
                    var val = if (c.retVal) |v| v else unreachable;
                    result.* = val.ref(true);
                    c.retVal = null;
                    return;
                },
                else => return err,
            }
        };
        if (func.isInitializer) {
            result.* = func.closure.getAt(0, "this");
            return;
        }
        result.* = .nil;
    }

    fn call(self: *Self, c: anytype, tok: Token, callee: Value, arguments: []Value) Error!Value {
        _ = self;
        _ = c;
        const arity = funcArity(callee);
        if (arguments.len != arity) {
            try c.errSet(tok, "Expected {d} arguments but got {d}.", .{ arity, arguments.len });
            return error.RuntimeError;
        }
        var result: Value = undefined;
        switch (callee) {
            .loxFunc => |*f| {
                try self.loxCall(c, f, arguments, &result);
            },
            .nativeFunc => |*f| {
                f.call(self, arguments, &result);
            },
            .loxClass => |f| {
                var instance = try LoxInstance.init(self.allocator, f);
                if (f.findMethod("init")) |initializer| {
                    var func = try initializer.bind(self.allocator, instance);
                    _ = try self.call(c, tok, Value{ .loxFunc = func }, arguments);
                }
                return Value{ .loxInstance = instance };
            },
            else => {
                try c.errSet(tok, "Can only call functions and classes.", .{});
                return error.RuntimeError;
            },
        }
        return result;
    }

    pub fn eval(self: *Self, c: anytype, exp: *const Expr) Error!Value {
        return switch (exp.*) {
            .binary => |*e| try self.evalBinary(c, e),
            .call => |*e| try self.evalCall(c, e),
            .get => |*e| try self.evalGet(c, e),
            .set => |*e| try self.evalSet(c, e),
            .this => |*e| try self.evalThis(c, e),
            .logical => |*e| try self.evalLogical(c, e),
            .grouping => |*e| try self.eval(c, e.expression),
            .literal => |*e| e.value,
            .unary => |*e| try self.evalUnary(c, e),
            .variable => |*e| try self.evalVar(c, e),
            .assign => |*e| try self.evalAssign(c, e),
        };
    }

    fn execBlock(self: *Self, c: anytype, statements: ArrayList(Stmt), env: *Environment) !void {
        var previous = self.env;
        self.env = env;
        defer {
            self.env = previous;
        }
        for (statements.items) |*stm| {
            try self.exec(c, stm);
        }
    }

    pub fn exec(self: *Self, c: anytype, stm: *const Stmt) Error!void {
        if (self.scratchLen > 0x1000) {
            self.scratchReset();
        }
        switch (stm.*) {
            .function => |f| {
                const function = Value{ .loxFunc = .{
                    .declaration = f,
                    .closure = self.env.ref(),
                    .ret = 0,
                    .isInitializer = false,
                } };
                try self.env.define(f.name.lexeme, function);
            },
            .class => |*s| {
                try self.env.define(s.name.lexeme, Value{ .nil = {} });
                var class = try self.funcArena.allocator.create(LoxClass);
                var methods = StringHashMap(LoxFunc).init(&self.funcArena.allocator);
                for (s.methods.items) |method| {
                    var function = LoxFunc{
                        .declaration = method,
                        .closure = self.env,
                        .ret = 0,
                        .isInitializer = std.mem.eql(u8, method.name.lexeme, "init"),
                    };
                    try methods.put(method.name.lexeme, function);
                }
                class.* = .{ .name = s.name.lexeme, .methods = methods };
                const val = Value{ .loxClass = class };
                try self.env.assign(c, s.name, val);
            },
            .expression => |e| {
                (try self.eval(c, e)).unref();
            },
            .print => |e| {
                var val = try self.eval(c, e);
                defer val.unref();
                try stringify(c.w, val);
                try c.w.print("\n", .{});
            },
            .varDecl => |*v| {
                var val: Value = Value{ .nil = {} };
                if (v.initializer) |ini| {
                    val = try self.eval(c, ini);
                }
                try self.env.define(v.name.lexeme, val.ref(false));
            },
            .block => |stms| {
                var env = try self.allocator.create(Environment);
                env.* = Environment.init(self.allocator, self.env);
                defer {
                    if (env.deinit()) {
                        // std.debug.print("DBG int.destroy env{*}\n", .{env});
                        self.allocator.destroy(env);
                    }
                }
                try self.execBlock(c, stms, env);
            },
            .ifStmt => |ifStmt| {
                var cond = try self.eval(c, ifStmt.condition);
                defer cond.unref();
                if (isTruthy(cond)) {
                    try self.exec(c, ifStmt.thenBranch);
                } else {
                    if (ifStmt.elseBranch) |elseBranch| {
                        try self.exec(c, elseBranch);
                    }
                }
            },
            .retStmt => |retStmt| {
                c.retVal = try self.eval(c, retStmt.value);
                return error.Return;
            },
            .whileStmt => |whileStmt| {
                var cond = try self.eval(c, whileStmt.condition);
                defer cond.unref();
                while (isTruthy(cond)) {
                    try self.exec(c, whileStmt.body);
                }
            },
        }
    }

    pub fn stringify(w: anytype, val: Value) Error!void {
        switch (val) {
            .loxFunc => |v| try w.print("<fn {s}>", .{v.declaration.name.lexeme}),
            .loxClass => |v| try w.print("<class {s}>", .{v.name}),
            .loxInstance => |v| try w.print("<instance {s}>", .{v.class.name}),
            .nativeFunc => |v| try w.print("<native fn {s}>", .{v}),
            .boolean => |v| try w.print("{s}", .{v}),
            .number => |v| {
                try w.print("{d}", .{v});
            },
            .string => |v| try w.print("{s}", .{v}),
            .nil => try w.print("nil", .{}),
        }
    }

    pub fn interpret(self: *Self, w: anytype, stms: []const Stmt) !void {
        var ctx = Context(@TypeOf(w)).init(w);
        for (stms) |*stm| {
            self.exec(&ctx, stm) catch |e| {
                switch (e) {
                    error.RuntimeError => {
                        reportRuntimeError(ctx.err.?);
                        return;
                    },
                    else => return e,
                }
            };
        }
    }
};

test "interpreter" {
    const scanner = @import("scanner.zig");
    const parser = @import("parser.zig");

    const Scanner = scanner.Scanner;
    const Parser = parser.Parser;

    // var allocator = std.testing.allocator;
    // NOTE: At this point I figured out that managing memory by reference
    // counting was too cumbersome, so I decided to stop freeing memory for
    // dynamic objects, and approach this in part2 with the garbage collector.
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;

    // const src = "var a = 1; var b = 2; print a + b; { var a = 5; print a; } print a; print clock();";
    // const src = "var a = \"foo\"; { var a = \"bar\"; } ";
    // const src = "fun foo(a) { print a + 1; } foo(2);";
    // const src = "fun foo() { print \"hello world\"; } foo();";
    // const src =
    //     \\fun makeCounter() {
    //     \\  print "makeCounter";
    //     \\  var i = 0;
    //     \\  fun count() {
    //     \\    print "count";
    //     \\    i = i + 1;
    //     \\    print i;
    //     \\  }
    //     \\
    //     \\  return count;
    //     \\}
    //     \\
    //     \\fun baz() {
    //     \\  return makeCounter();
    //     \\}
    //     \\
    //     \\var x;
    //     \\fun bar() {
    //     \\  print "AAA";
    //     \\  x = baz();
    //     \\  print "BBB";
    //     \\  return x;
    //     \\  // fun count() {
    //     \\  //   print "hello";
    //     \\  // }
    //     \\  // return count;
    //     \\}
    //     \\print "CCC";
    //     \\
    //     \\var counter = bar();
    //     \\// bar();
    //     \\// var counter = x;
    //     \\print "DDD";
    //     \\counter(); // "1".
    //     \\counter(); // "2".
    // ;
    // const src =
    //     \\class DevonshireCream {
    //     \\  serveOn() {
    //     \\    return "Scones";
    //     \\  }
    //     \\}
    //     \\
    //     \\print DevonshireCream; // Prints "DevonshireCream".
    // ;
    // const src =
    //     \\class Bagel {}
    //     \\var bagel = Bagel();
    //     \\var foo = bagel;
    //     \\print bagel; // Prints "Bagel instance".
    // ;
    // const src =
    //     \\class Bacon {
    //     \\  eat() {
    //     \\    print "Crunch crunch crunch!";
    //     \\  }
    //     \\}
    //     \\
    //     \\Bacon().eat(); // Prints "Crunch crunch crunch!".
    // ;
    const src =
        \\class Thing {
        \\  getCallback() {
        \\    fun localFunction() {
        \\      print this;
        \\    }
        \\
        \\    return localFunction;
        \\  }
        \\}
        \\
        \\var callback = Thing().getCallback();
        \\callback();
    ;
    var s = try Scanner.init(allocator, src);
    defer s.deinit();
    var tokens = try s.scanTokens();

    var int = try Interpreter.init(allocator);
    defer int.deinit();
    var p = try Parser.init(allocator, &int.funcArena, tokens);
    defer p.deinit();
    var statements = try p.parse();
    // std.debug.print("{s}\n", .{statements});

    var w = std.io.getStdErr().writer();

    var res = try Resolver.init(&int, allocator);
    defer res.deinit();
    try res.resolve(statements.items);
    try int.interpret(w, statements.items);

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
