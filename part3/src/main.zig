const std = @import("std");

const common = @import("common.zig");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");

const OpCode = common.OpCode;

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
    var ch = chunk.Chunk.init();
    const constant = ch.addConstant(1.2);
    ch.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    ch.write(@intCast(u8, constant), 123);
    ch.write(@enumToInt(OpCode.OP_RETURN), 123);
    debug.disassembleChunk(&ch, "test chunk");
    ch.deinit();
}
