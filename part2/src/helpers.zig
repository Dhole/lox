const std = @import("std");

const FmtOpts = std.fmt.FormatOptions;
const bufPrint = std.fmt.bufPrint;

const token = @import("token.zig");
const expr = @import("expr.zig");

const Token = token.Token;
const TT = token.TokenType;
const Value = expr.Value;

pub var hadError = false;
pub var hadRuntimeError = false;

// Originally "error"
pub fn printLineErr(line: u32, msg: []const u8) void {
    report(line, "", msg);
}

// Originally "error"
pub fn printTokenErr(t: Token, message: []const u8) void {
    if (t.type == TT.EOF) {
        report(t.line, "at end", message);
    } else {
        const b_len = 32;
        var b: [b_len]u8 = undefined;
        const where = std.fmt.bufPrint(&b, "at '{s}'", .{t.lexeme}) catch
            blk: {
            std.mem.copy(u8, b[b_len - 3 ..], "..'");
            break :blk &b;
        };
        report(t.line, where, message);
    }
}

// Originally "report"
pub fn report(line: u32, where: []const u8, msg: []const u8) void {
    std.log.err("[line {d}] Error {s}: {s}", .{ line, where, msg });
    hadError = true;
}

pub const RuntimeError = struct {
    const Self = @This();
    tok: Token,
    buf: [128]u8,
    msg: []const u8,

    // pub fn init(tok: Token, comptime fmt: []const u8, args: anytype) !Self {
    //     const msg = try bufPrint(c.err.buf, "Expected {d} arguments but got {d}.", .{ function_arity, arguments.len });
    // }

    pub fn format(self: Self, comptime fmt: []const u8, opts: FmtOpts, w: anytype) !void {
        _ = opts;
        _ = fmt;
        try w.print("{{token:{s}, msg:{s}}}", .{ self.tok, self.msg });
    }

    pub fn set(self: *Self, tok: Token, msg: []const u8) void {
        self.tok = tok;
        self.msg = msg;
    }
};

// Originally "runtimeError"
pub fn reportRuntimeError(e: RuntimeError) void {
    std.log.err("[line {d}] {s}", .{ e.tok.line, e.msg });
    hadRuntimeError = true;
}

pub fn Context(comptime Writer: type) type {
    return struct {
        const Self = @This();
        w: Writer,
        err: ?RuntimeError,
        retVal: ?Value,

        pub fn init(w: Writer) Self {
            return Self{
                .w = w,
                .err = null,
                .retVal = null,
            };
        }

        pub fn errSet(self: *Self, tok: Token, comptime fmt: []const u8, args: anytype) !void {
            self.err = .{ .tok = undefined, .buf = undefined, .msg = undefined };
            if (self.err) |*err| {
                err.tok = tok;
                err.msg = try bufPrint(&err.buf, fmt, args);
            } else unreachable;
        }
    };
}
