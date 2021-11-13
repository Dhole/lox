const memory = @import("memory.zig");

const growCapacity = memory.growCapacity;
const growArray = memory.growArray;
const freeArray = memory.freeArray;

pub const OpCode = enum {
    OP_RETURN,
};

pub const Chunk = struct {
    const Self = @This();

    count: usize,
    // capacity: usize, // capacity is code.len
    code: []u8,

    pub fn _init(self: *Self) void {
        self.* = Self{
            .count = 0,
            .code = &[_]u8{},
        };
    }

    pub fn init() Self {
        var self = Self{
            .count = undefined,
            .code = undefined,
        };
        self._init();
        return self;
    }

    pub fn deinit(self: *Self) void {
        freeArray(u8, self.code);
        self._init();
    }

    pub fn write(self: *Self, byte: u8) void {
        if (self.code.len < self.count + 1) {
            self.code = growArray(u8, self.code, growCapacity(self.code.len));
        }
        self.code[self.count] = byte;
        self.count += 1;
    }
};

test "chunk" {
    const std = @import("std");

    var chunk = Chunk.init();
    defer chunk.deinit();
    // std.debug.print("len: {d}\n", .{chunk.code.len});
    chunk.write(42);
    chunk.write(43);
}
