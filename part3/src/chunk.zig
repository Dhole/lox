const std = @import("std");

const _memory = @import("memory.zig");
const _value = @import("value.zig");
const g = @import("global.zig");

const growCapacity = _memory.growCapacity;
const growArray = _memory.growArray;
const freeArray = _memory.freeArray;
const ValueArray = _value.ValueArray;
const Value = _value.Value;

pub const Chunk = struct {
    const Self = @This();

    count: usize,
    // capacity: usize, // capacity is code.len
    code: []u8,
    constants: ValueArray,
    lines: []usize,

    pub fn _init(self: *Self) void {
        self.* = Self{
            .count = 0,
            .code = &[_]u8{},
            .lines = &[_]usize{},
            .constants = ValueArray.init(),
        };
    }

    pub fn init() Self {
        var self = Self{
            .count = undefined,
            .code = undefined,
            .lines = undefined,
            .constants = undefined,
        };
        self._init();
        return self;
    }

    pub fn deinit(self: *Self) void {
        freeArray(u8, self.code);
        freeArray(usize, self.lines);
        self.count = 0;
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) void {
        if (self.code.len < self.count + 1) {
            const newCapacity = growCapacity(self.code.len);
            self.code = growArray(u8, self.code, newCapacity);
            self.lines = growArray(usize, self.lines, newCapacity);
        }
        self.code[self.count] = byte;
        self.lines[self.count] = line;
        self.count += 1;
    }

    pub fn addConstant(self: *Self, val: Value) usize {
        g.vm.push(val);
        self.constants.write(val);
        _ = g.vm.pop();
        return self.constants.count - 1;
    }
};

test "chunk" {
    // const std = @import("std");

    var chunk = Chunk.init();
    defer chunk.deinit();
    // std.debug.print("len: {d}\n", .{chunk.code.len});
    chunk.write(42);
    chunk.write(43);
    _ = chunk.addConstant(123);
}
