const std = @import("std");

const _memory = @import("memory.zig");

const print = std.debug.print;
const allocate = _memory.allocate;
const create = _memory.create;
const destroy = _memory.destroy;
const freeArray = _memory.freeArray;

pub const Objects = struct {
    const Self = @This();

    objects: ?*Obj,

    pub fn init() Self {
        return Self{
            .objects = null,
        };
    }

    // ALLOCATE_OBJ
    pub fn allocateObject(self: *Self, comptime T: type, objectType: ObjType) *T {
        var object = @ptrCast(*Obj, create(T));
        object.type = objectType;
        object.next = self.objects;
        self.objects = object;
        return @ptrCast(*T, object);
    }

    pub fn allocateString(self: *Self, chars: []u8) *ObjString {
        var string = self.allocateObject(ObjString, ObjType.string);
        string.chars = chars;
        return string;
    }

    pub fn copyString(self: *Self, chars: []const u8) *ObjString {
        var heapChars = allocate(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);
        return self.allocateString(heapChars);
    }

    pub fn takeString(self: *Self, chars: []u8) *ObjString {
        return self.allocateString(chars);
    }

    // freeObjects
    pub fn deinit(self: *Self) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.deinit();
            object = next;
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
};

pub const ObjType = enum {
    string,
};

pub const Obj = struct {
    const Self = @This();

    type: ObjType,
    next: ?*Obj,

    pub fn asString(self: *Self) *ObjString {
        return @ptrCast(*ObjString, self);
    }

    // freeObject
    pub fn deinit(self: *Self) void {
        switch (self.type) {
            ObjType.string => {
                var string = @ptrCast(*ObjString, self);
                freeArray(u8, string.chars);
                destroy(string);
            },
        }
    }
};

pub fn printObject(obj: *Obj) void {
    switch (obj.type) {
        ObjType.string => print("\"{s}\"", .{obj.asString().chars}),
    }
}

// pub const Obj = struct {
//     type: ObjType,
//     value: ObjValue,
// };
