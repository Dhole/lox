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

    pub fn init(allocator: *Allocator, enclosing: ?*Self) Self {
        return Self{
            .allocator = allocator,
            .enclosing = enclosing,
            .values = StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var iterator = self.values.iterator();
        while (iterator.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.free(self.allocator);
        }
        self.values.deinit();
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
        if (self.values.getEntry(name.lexeme)) |entry| {
            entry.value_ptr.free(self.allocator);
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
        if (self.values.get(name.lexeme)) |val| {
            return val;
        } else {
            if (self.enclosing) |enclosing| {
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
