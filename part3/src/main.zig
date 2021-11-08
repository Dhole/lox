const std = @import("std");

const common = @import("common.zig");
const chunk = @import("chunk.zig");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}
