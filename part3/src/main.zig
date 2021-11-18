const std = @import("std");

const _common = @import("common.zig");
const _chunk = @import("chunk.zig");
const _debug = @import("debug.zig");
const _vm = @import("vm.zig");

const Chunk = _chunk.Chunk;
const VM = _vm.VM;
const disassembleChunk = _debug.disassembleChunk;
const OpCode = _common.OpCode;

const MAX_FILE_SIZE = 0x1000000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = &gpa.allocator;

fn main() anyerror!u8 {
    var vm = VM(true).init();

    const args = std.os.argv[1..std.os.argv.len];
    if (args.len() == 0) {
        return try repl();
    } else if (args.len() == 1) {
        return try runFile(args[1]);
    } else {
        std.log.err("Usage: clox [path]", .{});
        return 64;
    }

    vm.deinit();
    return 0;
}

fn repl() !u8 {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    const reader = std.io.bufferedReader(stdin.reader()).reader();

    var line: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("> ");
        const line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
        if (line) |l| {
            interpret(l);
        } else {
            return 0;
        }
    }
}

fn runFile(path: []const u8) !u8 {
    var file = try std.fs.cwd().openFile(path, .{ .read = true });
    const source = try path_file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(source);
    const result = interpret(source);
    switch (result) {
        INTERPRET_COMPILE_ERROR => return 65,
        INTERPRET_RUNTIME_ERROR => return 70,
    }
    return 0;
}
