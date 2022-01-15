const std = @import("std");

const _memory = @import("memory.zig");
const _common = @import("common.zig");
const _table = @import("table.zig");
const _chunk = @import("chunk.zig");
const _value = @import("value.zig");
const g = @import("global.zig");

const print = std.debug.print;
const allocate = _memory.allocate;
const sysReallocate = _memory.sysReallocate;
const create = _memory.create;
const destroy = _memory.destroy;
const freeArray = _memory.freeArray;
const Table = _table.Table;
const Chunk = _chunk.Chunk;
const Value = _value.Value;
const printValue = _value.printValue;
const debugStressGC = _memory.debugLogGC;
const debugLogGC = _memory.debugLogGC;
const growCapacity = _memory.growCapacity;
const flags = _common.flags;

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
        object.isMarked = false;
        object.next = self.objects;
        self.objects = object;

        if (debugLogGC) {
            print("{*} allocate {d} for {}\n", .{ object, @sizeOf(T), objectType });
        }
        return @ptrCast(*T, object);
    }

    pub fn allocateString(self: *Self, chars: []u8, hash: u32) *ObjString {
        var string = self.allocateObject(ObjString, ObjType.string);
        string.chars = chars;
        string.hash = hash;
        g.vm.push(.{ .obj = string.asObj() });
        _ = self.strings.set(string, .nil);
        _ = g.vm.pop();
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

pub const ObjUpvalue = struct {
    const Self = @This();

    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn init(objects: *Objects, slot: *Value) *Self {
        var upvalue = objects.allocateObject(ObjUpvalue, ObjType.upvalue);
        upvalue.closed = .nil;
        upvalue.location = slot;
        upvalue.next = null;
        return upvalue;
    }

    pub fn asObj(self: *Self) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const ObjClosure = struct {
    const Self = @This();

    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn init(objects: *Objects, function: *ObjFunction) *Self {
        var upvalues = allocate(?*ObjUpvalue, function.upvalueCount);
        for (upvalues) |*upvalue| {
            upvalue.* = null;
        }
        var closure = objects.allocateObject(ObjClosure, ObjType.closure);
        closure.function = function;
        closure.upvalues = upvalues;
        return closure;
    }

    pub fn asObj(self: *Self) *Obj {
        return @ptrCast(*Obj, self);
    }
};

pub const ObjType = enum {
    closure,
    function,
    native,
    string,
    upvalue,
};

pub const Obj = struct {
    const Self = @This();

    type: ObjType,
    isMarked: bool,
    next: ?*Obj,

    pub fn asString(self: *Self) *ObjString {
        return @ptrCast(*ObjString, self);
    }

    pub fn asUpvalue(self: *Self) *ObjUpvalue {
        return @ptrCast(*ObjUpvalue, self);
    }

    pub fn asFunction(self: *Self) *ObjFunction {
        return @ptrCast(*ObjFunction, self);
    }

    pub fn asNative(self: *Self) *ObjNative {
        return @ptrCast(*ObjNative, self);
    }

    pub fn asClosure(self: *Self) *ObjClosure {
        return @ptrCast(*ObjClosure, self);
    }

    pub fn mark(self: *Self) void {
        if (self.isMarked) {
            return;
        }
        if (debugLogGC) {
            print("{*} mark ", .{self});
            printValue(.{ .obj = self });
            print("\n", .{});
        }
        self.isMarked = true;

        if (g.vm.grayStack.len < g.vm.grayCount + 1) {
            g.vm.grayStack = sysReallocate(*Obj, g.vm.grayStack, growCapacity(g.vm.grayStack.len));
        }
        g.vm.grayStack[g.vm.grayCount] = self;
        g.vm.grayCount += 1;
    }

    pub fn blacken(self: *Self) void {
        if (debugLogGC) {
            print("{*} blacken ", .{self});
            printValue(.{ .obj = self });
            print("\n", .{});
        }
        switch (self.type) {
            ObjType.native => {},
            ObjType.string => {},
            ObjType.upvalue => {
                self.asUpvalue().closed.mark();
            },
            ObjType.function => {
                var function = self.asFunction();
                if (function.name) |name| {
                    name.asObj().mark();
                }
                function.chunk.constants.mark();
            },
            ObjType.closure => {
                var closure = self.asClosure();
                closure.function.asObj().mark();
                var i: usize = 0;
                while (i < closure.function.upvalueCount) : (i += 1) {
                    if (closure.upvalues[i]) |upvalue| {
                        upvalue.asObj().mark();
                    }
                }
            },
        }
    }

    // freeObject
    pub fn deinit(self: *Self) void {
        if (debugLogGC) {
            print("{*} free type {} ", .{ self, self.type });
            printValue(.{ .obj = self });
            print("\n", .{});
        }
        switch (self.type) {
            ObjType.closure => {
                var closure = self.asClosure();
                freeArray(?*ObjUpvalue, closure.upvalues);
                destroy(closure);
            },
            ObjType.function => {
                var function = self.asFunction();
                function.chunk.deinit();
                destroy(function);
            },
            ObjType.native => {
                var native = self.asNative();
                destroy(native);
            },
            ObjType.string => {
                var string = self.asString();
                freeArray(u8, string.chars);
                destroy(string);
            },
            ObjType.upvalue => {
                var upvalue = self.asUpvalue();
                destroy(upvalue);
            },
        }
    }
};

pub fn printObject(obj: *Obj) void {
    switch (obj.type) {
        ObjType.closure => printFunction(obj.asClosure().function),
        ObjType.function => printFunction(obj.asFunction()),
        ObjType.native => print("<native fn>", .{}),
        ObjType.string => print("\"{s}\"", .{obj.asString().chars}),
        ObjType.upvalue => print("upvalue", .{}),
    }
}

fn printFunction(function: *ObjFunction) void {
    if (function.name) |name| {
        print("<fn {s}>", .{name.chars});
    } else {
        print("<script>", .{});
    }
}

pub const ObjFunction = struct {
    const Self = @This();

    obj: Obj,
    arity: usize,
    upvalueCount: usize,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn init(objects: *Objects) *Self {
        var function = objects.allocateObject(ObjFunction, ObjType.function);
        function.arity = 0;
        function.upvalueCount = 0;
        function.name = null;
        function.chunk = Chunk.init();
        return function;
    }

    pub fn asObj(self: *Self) *Obj {
        return @ptrCast(*Obj, self);
    }
};

// pub const Obj = struct {
//     type: ObjType,
//     value: ObjValue,
// };

pub const NativeFn = fn (argCount: usize, args: []Value) Value;

pub const ObjNative = struct {
    const Self = @This();

    obj: Obj,
    function: NativeFn,

    pub fn init(objects: *Objects, function: NativeFn) *Self {
        var native = objects.allocateObject(ObjNative, ObjType.native);
        native.function = function;
        return native;
    }

    pub fn asObj(self: *Self) *Obj {
        return @ptrCast(*Obj, self);
    }
};
