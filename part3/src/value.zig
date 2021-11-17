const std = @import("std");

const _memory = @import("memory.zig");

const growCapacity = _memory.growCapacity;
const growArray = _memory.growArray;
const freeArray = _memory.freeArray;
const print = std.debug.print;

pub const Value = f64;

pub fn printValue(val: Value) void {
    print("\'{d}\'", .{val});
}

pub const ValueArray = struct {
    const Self = @This();

    count: usize,
    // capacity: usize, // capacity is values.len
    values: []Value,

    pub fn _init(self: *Self) void {
        self.* = Self{
            .count = 0,
            .values = &[_]Value{},
        };
    }

    pub fn init() Self {
        var self = Self{
            .count = undefined,
            .values = undefined,
        };
        self._init();
        return self;
    }

    pub fn deinit(self: *Self) void {
        freeArray(Value, self.values);
        self._init();
    }

    pub fn write(self: *Self, value: Value) void {
        if (self.values.len < self.count + 1) {
            self.values = growArray(Value, self.values, growCapacity(self.values.len));
        }
        self.values[self.count] = value;
        self.count += 1;
    }
};

test "value" {
    // const std = @import("std");

    var values = ValueArray.init();
    defer values.deinit();
    // std.debug.print("len: {d}\n", .{chunk.code.len});
    values.write(42.0);
    values.write(43.0);
}
