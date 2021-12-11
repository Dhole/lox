const std = @import("std");

const _memory = @import("memory.zig");
const _object = @import("object.zig");

const growCapacity = _memory.growCapacity;
const growArray = _memory.growArray;
const freeArray = _memory.freeArray;
const print = std.debug.print;
const Obj = _object.Obj;
const ObjType = _object.ObjType;
const printObject = _object.printObject;

pub const ValueType = enum {
    boolean,
    nil,
    number,
    obj,
};

pub const Value = union(ValueType) {
    const Self = @This();

    boolean: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub fn initBool(value: bool) Self {
        return Self{ .boolean = value };
    }

    pub fn initNumber(value: f64) Self {
        return Self{ .number = value };
    }

    pub fn initNil() Self {
        return Self.nil;
    }

    pub fn initObj(object: *Obj) Self {
        return Self{ .obj = object };
    }

    pub fn isBool(self: *const Self) bool {
        return self.* == ValueType.boolean;
    }

    pub fn isNumber(self: *const Self) bool {
        return self.* == ValueType.number;
    }

    pub fn isNil(self: *const Self) bool {
        return self.* == ValueType.nil;
    }

    pub fn isObj(self: *const Self) bool {
        return self.* == ValueType.obj;
    }

    pub fn isString(self: *const Self) bool {
        return switch (self.*) {
            Value.obj => |obj| obj.type == ObjType.string,
            else => false,
        };
    }

    pub fn equals(self: *const Self, b: Value) bool {
        if (@as(ValueType, self.*) != @as(ValueType, b)) {
            return false;
        }
        return switch (self.*) {
            Value.boolean => |a| a == b.boolean,
            Value.nil => false,
            Value.number => |a| a == b.number,
            Value.obj => |a| a == b.obj,
        };
    }
};

fn booleanStr(boolean: bool) []const u8 {
    return if (boolean) ("true")[0..] else ("false")[0..];
}

pub fn printValue(val: Value) void {
    switch (val) {
        Value.boolean => |boolean| print("{s}", .{booleanStr(boolean)}),
        Value.nil => print("nil", .{}),
        Value.number => |number| print("{d}", .{number}),
        Value.obj => |obj| printObject(obj),
    }
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
