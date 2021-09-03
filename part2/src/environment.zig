const std = @import("std");

const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const expr = @import("expr.zig");
const helpers = @import("helpers.zig");

const Token = token.Token;
const TT = token.TokenType;
const Value = expr.Value;
const Context = helpers.Context;
const RuntimeError = helpers.RuntimeError;

const Error = error{RuntimeError};

pub const Environment = struct {
    const Self = @This();
    allocator: *Allocator,
    enclosing: ?*Self,
    values: StringHashMap(Value),
    refs: i32, // Number of references in closures

    pub fn init(allocator: *Allocator, enclosing: ?*Self) Self {
        return Self{
            .allocator = allocator,
            .enclosing = enclosing,
            .values = StringHashMap(Value).init(allocator),
            .refs = 1,
        };
    }

    // Returns true if deinit frees the Environment
    pub fn deinit(self: *Self) bool {
        // std.debug.print("DBG env{*}.deinit {{ .refs = {d} }}\n", .{ self, self.refs });
        if (self.refs < 0) {
            @panic("env.deinit oh no");
        }
        var iterator = self.values.iterator();
        while (iterator.next()) |entry| {
            entry.value_ptr.unref();
        }
        self.refs -= 1;
        if (self.refs > 0) {
            return false;
        }
        self.free();
        return true;
    }

    fn free(self: *Self) void {
        var iterator = self.values.iterator();
        while (iterator.next()) |entry| {
            entry.value_ptr.free(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.values.deinit();
    }

    pub fn ref(self: *Self) *Self {
        self.refs += 1;
        // std.debug.print("DBG env{*}.ref {{ .refs = {d}, .enclosing = {*} }}\n", .{ self, self.refs, self.enclosing });
        // std.debug.dumpCurrentStackTrace(null);
        if (self.enclosing) |enclosing| {
            _ = enclosing.ref();
        }
        return self;
    }

    pub fn unref(self: *Self) bool {
        self.refs -= 1;
        if (self.refs < 0) {
            @panic("env.unref oh no");
        }
        // std.debug.print("DBG env{*}.unref {{ .refs = {d} }}\n", .{ self, self.refs });
        if (self.enclosing) |enclosing| {
            if (enclosing.unref()) {
                enclosing.allocator.destroy(enclosing);
            }
        }
        if (self.refs > 0) {
            return false;
        }
        self.free();
        return true;
    }

    pub fn define(self: *Self, name: []const u8, value: Value) !void {
        if (self.values.getEntry(name)) |entry| {
            entry.value_ptr.free(self.allocator);
            entry.value_ptr.* = try value.clone(self.allocator);
        } else {
            try self.values.put(try self.allocator.dupe(u8, name), try value.clone(self.allocator));
        }
    }

    pub fn assign(self: *Self, c: anytype, name: Token, value: Value) !void {
        // std.debug.print("DBG env.assign {s}\n", .{name});
        if (self.values.getEntry(name.lexeme)) |entry| {
            var old = entry.value_ptr.*;
            defer {
                old.unref();
                old.free(self.allocator);
            }
            entry.value_ptr.* = try value.clone(self.allocator);
        } else {
            if (self.enclosing) |enclosing| {
                return enclosing.assign(c, name, value);
            }
            try c.errSet(name, "Undefined variable.", .{});
            return error.RuntimeError;
        }
    }

    pub fn get(self: *Self, c: anytype, name: Token) !Value {
        // std.debug.print("DBG env.get {s}\n", .{name});
        if (self.values.get(name.lexeme)) |val| {
            return val;
        } else {
            if (self.enclosing) |enclosing| {
                // std.debug.print("DBG env.enclosing = {*}\n", .{self.enclosing});
                return enclosing.get(c, name);
            }
            try c.errSet(name, "Undefined variable.", .{});
            return error.RuntimeError;
        }
    }
};

test "environment" {
    var env = Environment.init(std.testing.allocator, null);
    defer env.deinit();
    var ctx: Context = Context.init();

    const name = "foo";
    try env.define(name, Value{ .number = 42 });
    var val = try env.get(&ctx, Token{ .type = TT.IDENTIFIER, .lexeme = "foo", .literal = null, .line = 0 });
    std.debug.print("{s}\n", .{val});
}
