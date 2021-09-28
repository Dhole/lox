const std = @import("std");

const StringHashMap = std.StringHashMap;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const interpreter = @import("interpreter.zig");
const environment = @import("environment.zig");

const Token = token.Token;
const Lit = token.Token.Literal;
const TT = token.TokenType;
const Interpreter = interpreter.Interpreter;
const Environment = environment.Environment;

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
    loxClass,
    loxInstance,
    nativeFunc,
    boolean,
    number,
    string,
    nil,
};

// pub const NativeFunc = enum {
//     clock,
// };

pub const NativeFunc = struct {
    // name: []const u8,
    arity: u32,
    call: fn (interpreter: *Interpreter, arguments: []Value, result: *Value) void,
};

pub const LoxFunc = struct {
    ret: u32,
    closure: *Environment,
    declaration: Function,
};

pub const LoxClass = struct {
    name: []const u8,
};

pub const LoxInstance = struct {
    const Self = @This();
    allocator: *Allocator,
    class: *const LoxClass,
    refs: u32,
    ret: u32,
    fields: StringHashMap(Value),

    pub fn init(allocator: *Allocator, class: *const LoxClass) !*Self {
        var instance = try allocator.create(LoxInstance);
        instance.* = Self{
            .allocator = allocator,
            .class = class,
            .fields = StringHashMap(Value).init(allocator),
            .refs = 1,
            .ret = 0,
        };
        return instance;
    }

    pub fn deinit(self: *Self) void {
        // TODO: Maybe unref each field?
        var iterator = self.fields.iterator();
        while (iterator.next()) |entry| {
            entry.value_ptr.free(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.fields.deinit();
        self.allocator.destroy(self);
    }

    pub fn get(self: *Self, c: anytype, name: Token) !Value {
        if (self.fields.get(name.lexeme)) |val| {
            return val;
        } else {
            try c.errSet(name, "Undefined property {s}.", .{name.lexeme});
            return error.RuntimeError;
        }
    }

    pub fn set(self: *Self, name: Token, value: Value) !void {
        if (self.fields.getEntry(name.lexeme)) |entry| {
            entry.value_ptr.free(self.allocator);
            entry.value_ptr.* = try value.clone(self.allocator);
        } else {
            try self.fields.put(try self.allocator.dupe(u8, name.lexeme), try value.clone(self.allocator));
        }
    }
};

// Values are either copy or reference.  Copy values need to be copied in clone
// and freed in free.  Reference values need to increase a reference in ref,
// and decrease a reference (and free if refs == 0) in unref.
pub const Value = union(ValueTag) {
    loxFunc: LoxFunc,
    loxClass: *LoxClass,
    loxInstance: *LoxInstance,
    nativeFunc: NativeFunc,
    boolean: bool,
    number: f64,
    string: []const u8,
    nil,

    // clone a copy value
    pub fn clone(self: *const Value, allocator: *Allocator) !Value {
        return switch (self.*) {
            .string => |s| blk: {
                var cpy = try allocator.dupe(u8, s);
                break :blk Value{ .string = cpy };
            },
            // .loxInstance => |i| blk: {
            //     std.debug.print("DBG {*}.clone refs: {d}\n", .{ i, i.refs });
            //     i.refs += 1;
            //     // var cpy = try allocator.create(LoxInstance);
            //     // cpy.* = i.*;
            //     break :blk Value{ .loxInstance = i };
            // },
            else => self.*,
        };
    }

    // free a copy value
    pub fn free(self: *Value, allocator: *Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            // .loxInstance => |i| allocator.destroy(i),
            else => {},
        }
    }

    // unref a reference value
    pub fn unref(self: *Value) void {
        switch (self.*) {
            .loxFunc => |*f| {
                // std.debug.print("DBG loxFunc{*}.unref\n", .{f});
                // std.debug.dumpCurrentStackTrace(null);
                if (f.closure.unref()) {
                    // std.debug.print("DBG val{{{s}}}.destroy env{*}\n", .{ f.declaration.name.lexeme, f.closure });
                    f.closure.allocator.destroy(f.closure);
                }
            },
            .loxInstance => |i| {
                i.refs -= 1;
                std.debug.print("DBG {*}.unref refs: {d}\n", .{ i, i.refs });
                if (i.refs == 0) {
                    // i.allocator.destroy(i);
                    i.deinit();
                }
            },
            else => {},
        }
    }

    // ref a reference value
    pub fn ref(self: *Value, ret: bool) Value {
        switch (self.*) {
            .loxFunc => |*f| {
                if (ret) {
                    f.ret += 1;
                } else if (!ret and f.ret > 0) {
                    f.ret = 0;
                    return self.*;
                }
                _ = f.closure.ref();
            },
            .loxInstance => |i| {
                std.debug.print("DBG {*}.ref refs: {d}\n", .{ i, i.refs });
                if (ret) {
                    i.ret += 1;
                } else if (!ret and i.ret > 0) {
                    i.ret = 0;
                    return self.*;
                }
                i.refs += 1;
            },
            else => {},
        }
        return self.*;
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

pub const Get = struct {
    name: Token,
    object: *const Expr,
};

pub const Set = struct {
    name: Token,
    object: *const Expr,
    value: *const Expr,
};

pub const Expr = union(enum) {
    binary: Binary,
    call: Call,
    get: Get,
    grouping: Grouping,
    literal: Literal,
    logical: Logical,
    set: Set,
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
            .nativeFunc => |v| try w.print("<native fun {s}>", .{v}),
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

pub const RetStmt = struct {
    keyword: Token,
    value: *Expr,
};

pub const Class = struct {
    name: Token,
    methods: ArrayList(Function),
};

pub const Stmt = union(enum) {
    block: ArrayList(Stmt),
    class: Class,
    expression: *Expr,
    function: Function,
    ifStmt: IfStmt,
    print: *Expr,
    retStmt: RetStmt,
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
