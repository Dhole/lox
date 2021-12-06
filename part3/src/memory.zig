const std = @import("std");

const allocator = std.heap.page_allocator;

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn reallocate(comptime T: type, slice: []T, newSize: usize) []T {
    if (newSize == 0) {
        allocator.free(slice);
        return &[_]T{};
    }
    var res = if (slice.len == 0) allocator.alloc(T, newSize) else allocator.realloc(slice, newSize);
    return res catch |err| {
        std.log.err("ERROR: {s}", .{err});
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
    return allocator.create(T) catch |err| {
        std.log.err("ERROR: {s}", .{err});
        std.os.exit(1);
    };
}

// FREE
pub fn destroy(ptr: anytype) void {
    allocator.destroy(ptr);
}
