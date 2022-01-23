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

const QNAN: u64 = 0x7ffc000000000000;
const SIGN_BIT: u64 = 0x8000000000000000;
const TAG_NIL: u64 = 1; // 01.
const TAG_FALSE: u64 = 2; // 10.
const TAG_TRUE: u64 = 3; // 11.

const nanBoxing: bool = true;

pub const ValueType = enum {
    boolean,
    nil,
    number,
    obj,
};

pub const Value = if (nanBoxing) ValueNanBoxing else ValueRegular;

pub const ValueNanBoxing = struct {
    const Self = @This();

    value: u64,

    pub fn initNumber(value: f64) Self {
        var cpy = value;
        return Self{ .value = @ptrCast(*u64, &cpy).* };
    }

    pub fn asNumber(self: Self) f64 {
        var cpy = self.value;
        return @ptrCast(*f64, &cpy).*;
    }

    pub fn isNumber(self: Self) bool {
        return (self.value & QNAN) != QNAN;
    }

    pub fn initBool(value: bool) Self {
        return Self{ .value = if (value)
            QNAN | TAG_TRUE
        else
            QNAN | TAG_FALSE };
    }

    pub fn isBool(self: Self) bool {
        return self.value | 1 == QNAN | TAG_TRUE;
    }

    pub fn asBool(self: Self) bool {
        return self.value == QNAN | TAG_TRUE;
    }

    pub fn initNil() Self {
        return Self{ .value = QNAN | TAG_NIL };
    }

    pub fn isNil(self: Self) bool {
        return self.value == QNAN | TAG_NIL;
    }

    pub fn asNil(self: Self) void {
        _ = self;
        return void;
    }

    pub fn initObj(object: *Obj) Self {
        return Self{ .value = @ptrToInt(object) | SIGN_BIT | QNAN };
    }

    pub fn isObj(self: Self) bool {
        return self.value & (SIGN_BIT | QNAN) == (SIGN_BIT | QNAN);
    }

    pub fn asObj(self: *const Self) *Obj {
        return @intToPtr(*Obj, self.value & ~(SIGN_BIT | QNAN));
    }

    pub fn isString(self: Self) bool {
        return if (self.isObj())
            self.asObj().type == ObjType.string
        else
            false;
    }

    pub fn isInstance(self: Self) bool {
        return if (self.isObj())
            self.asObj().type == ObjType.instance
        else
            false;
    }

    pub fn isClass(self: Self) bool {
        return if (self.isObj())
            self.asObj().type == ObjType.class
        else
            false;
    }

    pub fn equals(self: Self, b: Self) bool {
        if (self.isNumber() and b.isNumber()) {
            return self.asNumber() == b.asNumber();
        }
        return self.value == b.value;
    }

    pub fn mark(self: Self) void {
        if (self.isObj()) {
            self.asObj().mark();
        }
    }
};

pub const ValueRegular = union(ValueType) {
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

    pub fn isInstance(self: *const Self) bool {
        return switch (self.*) {
            Value.obj => |obj| obj.type == ObjType.instance,
            else => false,
        };
    }

    pub fn isClass(self: *const Self) bool {
        return switch (self.*) {
            Value.obj => |obj| obj.type == ObjType.class,
            else => false,
        };
    }

    pub fn asBool(self: *const Self) bool {
        return self.boolean;
    }

    pub fn asNil(self: *const Self) void {
        return self.nil;
    }

    pub fn asNumber(self: *const Self) f64 {
        return self.number;
    }

    pub fn asObj(self: *const Self) *Obj {
        return self.obj;
    }

    pub fn equals(self: *const Self, b: Self) bool {
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

    pub fn mark(self: Self) void {
        switch (self) {
            Self.obj => |obj| obj.mark(),
            else => {},
        }
    }
};

fn booleanStr(boolean: bool) []const u8 {
    return if (boolean) ("true")[0..] else ("false")[0..];
}

pub fn printValue(val: Value) void {
    if (val.isBool()) {
        print("{s}", .{booleanStr(val.asBool())});
    } else if (val.isNil()) {
        print("nil", .{});
    } else if (val.isNumber()) {
        print("{d}", .{val.asNumber()});
    } else if (val.isObj()) {
        printObject(val.asObj());
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

    pub fn mark(self: *Self) void {
        var i: usize = 0;
        while (i < self.count) : (i += 1) {
            self.values[i].mark();
        }
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
