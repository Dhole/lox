const std = @import("std");

const _common = @import("common.zig");
const _chunk = @import("chunk.zig");
const _debug = @import("debug.zig");
const _vm = @import("vm.zig");

const Chunk = _chunk.Chunk;
const VM = _vm.VM;
const InterpretResult = _vm.InterpretResult;
const disassembleChunk = _debug.disassembleChunk;
const OpCode = _common.OpCode;
const Flags = _common.Flags;

const MAX_FILE_SIZE = 0x1000000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = &gpa.allocator;

pub fn main() anyerror!u8 {
    const flags = Flags{ .debugTraceExecution = false, .debugPrintCode = false };
    var vm = VM(flags).init();
    defer vm.deinit();

    const args = std.os.argv[1..std.os.argv.len];
    if (args.len == 0) {
        return try repl(
            flags,
            &vm,
        );
    } else if (args.len == 1) {
        const arg0 = std.mem.sliceTo(args[0], '0');
        return try runFile(flags, &vm, arg0);
    } else {
        std.log.err("Usage: clox [path]", .{});
        return 64;
    }

    return 0;
}

fn repl(comptime flags: Flags, vm: *VM(flags)) !u8 {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    const reader = std.io.bufferedReader(stdin.reader()).reader();

    var buffer: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("> ");
        const line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
        if (line) |l| {
            _ = vm.interpret(l);
        } else {
            return 0;
        }
    }
}

fn runFile(comptime flags: Flags, vm: *VM(flags), path: []const u8) !u8 {
    var file = try std.fs.cwd().openFile(path, .{ .read = true });
    const source = try file.readToEndAlloc(allocator, MAX_FILE_SIZE);
    defer allocator.free(source);
    const result = vm.interpret(source);
    switch (result) {
        InterpretResult.COMPILE_ERROR => return 65,
        InterpretResult.RUNTIME_ERROR => return 70,
        InterpretResult.OK => return 0,
    }
}
