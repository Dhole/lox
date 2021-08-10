const std = @import("std");

const token = @import("token.zig");

const Token = token.Token;
const TT = token.TokenType;

pub var hadError = false;

// Originally "error"
pub fn printLineErr(line: u32, msg: []const u8) void {
    report(line, "", msg);
}

// Originally "error"
pub fn printTokenErr(t: Token, message: []const u8) void {
    if (t.type == TT.EOF) {
        report(t.line, " at end", message);
    } else {
        const b_len = 32;
        var b: [b_len]u8 = undefined;
        _ = std.fmt.bufPrint(&b, " at '{s}'", .{t.lexeme}) catch {
            std.mem.copy(u8, b[b_len - 3 ..], "..'");
        };
        report(t.line, &b, message);
    }
}

// Originally "report"
pub fn report(line: u32, where: []const u8, msg: []const u8) void {
    std.log.err("[line {d}] Error {s}: {s}", .{ line, where, msg });
    hadError = true;
}
