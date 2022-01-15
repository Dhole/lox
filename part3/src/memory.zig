const std = @import("std");

const _common = @import("common.zig");
const g = @import("global.zig");

pub const allocator = std.heap.page_allocator;
const flags = _common.flags;

pub const debugStressGC = true;
pub const debugLogGC = true;

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn sysReallocate(comptime T: type, slice: []T, newSize: usize) []T {
    if (newSize == 0) {
        allocator.free(slice);
        return &[_]T{};
    }
    var res = if (slice.len == 0) allocator.alloc(T, newSize) else allocator.realloc(slice, newSize);
    return res catch |err| {
        std.log.err("ERROR: sysReallocate: {s}", .{err});
        std.os.exit(1);
    };
}

pub fn reallocate(comptime T: type, slice: []T, newSize: usize) []T {
    g.vm.bytesAllocated = @intCast(usize, //
        @intCast(isize, g.vm.bytesAllocated) + //
        (@intCast(isize, newSize) - @intCast(isize, slice.len)) * @intCast(isize, @sizeOf(T)) //
    );
    if (newSize == 0) {
        allocator.free(slice);
        return &[_]T{};
    }
    if (newSize > slice.len) {
        if (debugStressGC) {
            g.vm.collectGarbage();
        }
    }
    if (g.vm.bytesAllocated > g.vm.nextGC) {
        g.vm.collectGarbage();
    }
    var res = if (slice.len == 0) allocator.alloc(T, newSize) else allocator.realloc(slice, newSize);
    return res catch |err| {
        std.log.err("ERROR: reallocate: {s}", .{err});
        std.os.exit(1);
    };
}

pub fn allocate(comptime T: type, size: usize) []T {
    return reallocate(T, &[_]T{}, size);
}

pub fn growArray(comptime T: type, slice: []T, newCount: usize) []T {
    return reallocate(T, slice, newCount);
}

pub fn freeArray(comptime T: type, slice: []T) void {
    _ = reallocate(T, slice, 0);
}

pub fn create(comptime T: type) *T {
    g.vm.bytesAllocated += @sizeOf(T);
    if (debugStressGC) {
        g.vm.collectGarbage();
    }
    if (g.vm.bytesAllocated > g.vm.nextGC) {
        g.vm.collectGarbage();
    }
    return allocator.create(T) catch |err| {
        std.log.err("ERROR: {s}", .{err});
        std.os.exit(1);
    };
}

// FREE
pub fn destroy(ptr: anytype) void {
    g.vm.bytesAllocated -= @sizeOf(@TypeOf(ptr.*));
    allocator.destroy(ptr);
}

const Foo = struct {
    a: usize,
    b: usize,
    c: usize,
};

test "memory" {
    const expect = @import("std").testing.expect;

    std.log.debug("test", .{});
    var ptr = &Foo{ .a = 0, .b = 1, .c = 2 };
    try expect(@sizeOf(Foo) == @sizeOf(@TypeOf(ptr.*)));
}
