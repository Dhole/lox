const std = @import("std");

const _memory = @import("memory.zig");
const _table = @import("table.zig");

const print = std.debug.print;
const allocate = _memory.allocate;
const create = _memory.create;
const destroy = _memory.destroy;
const freeArray = _memory.freeArray;
const Table = _table.Table;

fn hashString(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |c| {
        hash ^= c;
        _ = @mulWithOverflow(u32, hash, 16777619, &hash);
    }
    return hash;
}

pub const Objects = struct {
    const Self = @This();

    objects: ?*Obj,
    strings: Table,

    pub fn init() Self {
        return Self{
            .objects = null,
            .strings = Table.init(),
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

    pub fn allocateString(self: *Self, chars: []u8, hash: u32) *ObjString {
        var string = self.allocateObject(ObjString, ObjType.string);
        string.chars = chars;
        string.hash = hash;
        _ = self.strings.set(string, .nil);
        return string;
    }

    pub fn copyString(self: *Self, chars: []const u8) *ObjString {
        const hash = hashString(chars);
        if (self.strings.findString(chars, hash)) |interned| {
            return interned;
        }

        var heapChars = allocate(u8, chars.len);
        std.mem.copy(u8, heapChars, chars);
        return self.allocateString(heapChars, hash);
    }

    pub fn takeString(self: *Self, chars: []u8) *ObjString {
        const hash = hashString(chars);
        if (self.strings.findString(chars, hash)) |interned| {
            freeArray(u8, chars);
            return interned;
        }

        return self.allocateString(chars, hash);
    }

    // freeObjects
    pub fn deinit(self: *Self) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.deinit();
            object = next;
        }
        self.strings.deinit();
    }
};

pub const ObjString = struct {
    const Self = @This();

    obj: Obj,
    chars: []u8,
    hash: u32,

    pub fn asObj(self: *Self) *Obj {
        return @ptrCast(*Obj, self);
    }
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
                var string = self.asString();
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
