const std = @import("std");

const _memory = @import("memory.zig");

const growCapacity = _memory.growCapacity;
const growArray = _memory.growArray;
const freeArray = _memory.freeArray;
const print = std.debug.print;

pub const ValueType = enum {
    boolean,
    nil,
    number,
};

pub const Value = union(ValueType) {
    const Self = @This();

    boolean: bool,
    number: f64,
    nil: void,

    pub fn initBool(value: bool) Self {
        return Self{ .boolean = value };
    }

    pub fn initNumber(value: f64) Self {
        return Self{ .number = value };
    }

    pub fn initNil() Self {
        return Self.nil;
    }

    fn isBool(self: *Self) bool {
        return @as(ValueTYpe, self) == ValueType.boolean;
    }

    fn isNumber(self: *Self) bool {
        return @as(ValueTYpe, self) == ValueType.number;
    }

    fn isNil(self: *Self) bool {
        return @as(ValueTYpe, self) == ValueType.nil;
    }
};

pub fn printValue(val: Value) void {
    print("\'{d}\'", .{val.number});
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
