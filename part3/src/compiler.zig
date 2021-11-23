const std = @import("std");

const _scanner = @import("scanner.zig");

const print = std.debug.print;
const Scanner = _scanner.Scanner;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);

    var line = -1;
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            print("{d:0>4}", .{token.line});
            line = token.line;
        } else {
            print("   | ", .{});
        }
        print("{d:0>2} '{s}'\n", .{ token.type, token.lit });

        if (token.type == TOKEN_EOF) {
            break;
        }
    }
}
