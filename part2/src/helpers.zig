const std = @import("std");

pub var hadError = false;

// Originally "error"
pub fn printLineErr(line: u32, msg: []const u8) void {
    report(line, "", msg);
}

// Originally "report"
pub fn report(line: u32, where: []const u8, msg: []const u8) void {
    std.log.err("[line {d}] Error {s}: {s}", .{ line, where, msg });
    hadError = true;
}
