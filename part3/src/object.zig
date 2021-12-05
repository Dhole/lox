const std = @import("std");

const _memory = @import("memory.zig");

const print = std.debug.print;
const allocate = _memory.allocate;
const create = _memory.create;

fn allocateString(chars: []u8) *ObjString {
    var string = create(ObjString);
    string.chars = chars;
    return string;
}

pub fn copyString(chars: []const u8) *ObjString {
    var heapChars = allocate(u8, chars.len);
    std.mem.copy(u8, heapChars, chars);
    return allocateString(heapChars);
}

pub const ObjString = struct {
    chars: []u8,
};

pub const ObjType = enum {
    string,
};

pub const Obj = union(ObjType) {
    string: *ObjString,
};

pub fn printObject(obj: Obj) void {
    switch (obj) {
        Obj.string => |string| print("\"{s}\"", .{string.chars}),
    }
}

// pub const Obj = struct {
//     type: ObjType,
//     value: ObjValue,
// };
