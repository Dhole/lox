const std = @import("std");

const allocator = std.heap.page_allocator;

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

fn reallocate(comptime T: type, slice: []T, newSize: usize) []T {
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

pub fn growArray(comptime T: type, slice: []T, newCount: usize) []T {
    return reallocate(T, slice, newCount);
}

pub fn freeArray(comptime T: type, slice: []T) void {
    _ = reallocate(T, slice, 0);
}
