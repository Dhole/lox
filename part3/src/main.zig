const _common = @import("common.zig");
const _chunk = @import("chunk.zig");
const _debug = @import("debug.zig");
const _vm = @import("vm.zig");

const Chunk = _chunk.Chunk;
const VM = _vm.VM;
const disassembleChunk = _debug.disassembleChunk;

const OpCode = _common.OpCode;

pub fn main() anyerror!void {
    var vm = VM(true).init();
    var chunk = Chunk.init();
    var constant = chunk.addConstant(1.2);
    chunk.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    chunk.write(@intCast(u8, constant), 123);

    constant = chunk.addConstant(3.4);
    chunk.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    chunk.write(@intCast(u8, constant), 123);

    chunk.write(@enumToInt(OpCode.OP_ADD), 123);

    constant = chunk.addConstant(5.6);
    chunk.write(@enumToInt(OpCode.OP_CONSTANT), 123);
    chunk.write(@intCast(u8, constant), 123);

    chunk.write(@enumToInt(OpCode.OP_DIVIDE), 123);
    chunk.write(@enumToInt(OpCode.OP_NEGATE), 123);

    chunk.write(@enumToInt(OpCode.OP_RETURN), 123);
    // disassembleChunk(&chunk, "test chunk");
    _ = vm.interpret(&chunk);
    vm.deinit();
    chunk.deinit();
}
