const std = @import("std");

const token = @import("token.zig");
const scanner = @import("scanner.zig");
const expr = @import("expr.zig");
const parser = @import("parser.zig");
const helpers = @import("helpers.zig");

const Expr = expr.Expr;
const Parser = parser.Parser;

const MAX_FILE_SIZE = 0x1000000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !u8 {
    const args = std.os.argv[1..std.os.argv.len];
    if (args.len > 1) {
        std.log.err("Usage: jlox [script]", .{});
        return 64;
    } else if (args.len == 1) {
        const arg0 = std.mem.sliceTo(args[0], '0');
        return try runFile(arg0);
    } else {
        return try runPrompt();
    }
    return 0;
}

fn runFile(path: []u8) !u8 {
    std.log.info("path: {s}", .{path});

    var path_file = try std.fs.cwd().openFile(path, .{ .read = true });
    const bytes = try path_file.readToEndAlloc(&gpa.allocator, MAX_FILE_SIZE);
    defer gpa.allocator.free(bytes);

    try run(bytes);

    if (helpers.hadError) {
        return 65;
    }
    return 0;
}

fn runPrompt() !u8 {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    const reader = std.io.bufferedReader(stdin.reader()).reader();

    var buffer: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("> ");
        const line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
        if (line) |l| {
            try run(l);
            helpers.hadError = false;
        } else {
            return 0;
        }
    }
}

fn run(bytes: []u8) !void {
    // std.log.info("{any}", .{bytes});
    var s = try scanner.Scanner.init(&gpa.allocator, bytes);
    defer s.deinit();
    const tokens = try s.scanTokens();

    var exp: *Expr = undefined;
    var p = Parser.init(&gpa.allocator, tokens);
    defer p.deinit();

    if (p.parse()) |e| {
        exp = e;
    } else {
        return;
    }

    if (helpers.hadError) {
        return;
    }

    var w = std.io.getStdErr().writer();
    try expr.printAst(w, exp);
}
