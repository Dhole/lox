const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");

const print = std.debug.print;
const Chunk = _chunk.Chunk;
const OpCode = _common.OpCode;
const printValue = _value.printValue;
// const print = std.log.info;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{x:0>4} ", .{offset});
    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:0>4} ", .{chunk.lines[offset]});
    }

    const instruction = chunk.code[offset];
    return switch (instruction) {
        @enumToInt(OpCode.RETURN) => simpleInstruction("OP_RETURN", offset),
        @enumToInt(OpCode.CONSTANT) => constantInstruction("OP_CONSTANT", chunk, offset),
        @enumToInt(OpCode.NIL) => simpleInstruction("OP_NIL", offset),
        @enumToInt(OpCode.TRUE) => simpleInstruction("OP_TRUE", offset),
        @enumToInt(OpCode.FALSE) => simpleInstruction("OP_FALSE", offset),
        @enumToInt(OpCode.EQUAL) => simpleInstruction("OP_EQUAL", offset),
        @enumToInt(OpCode.GREATER) => simpleInstruction("OP_GREATER", offset),
        @enumToInt(OpCode.LESS) => simpleInstruction("OP_LESS", offset),
        @enumToInt(OpCode.NEGATE) => simpleInstruction("OP_NEGATE", offset),
        @enumToInt(OpCode.ADD) => simpleInstruction("OP_ADD", offset),
        @enumToInt(OpCode.SUBTRACT) => simpleInstruction("OP_SUBTRACT", offset),
        @enumToInt(OpCode.MULTIPLY) => simpleInstruction("OP_MULTIPLY", offset),
        @enumToInt(OpCode.DIVIDE) => simpleInstruction("OP_DIVIDE", offset),
        @enumToInt(OpCode.NOT) => simpleInstruction("OP_NOT", offset),
        else => blk: {
            print("Unknown opcode {x:0>2}\n", .{instruction});
            break :blk offset + 1;
        },
    };
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    print("{s} {d: >4} ", .{ name, constant });
    printValue(chunk.constants.values[constant]);
    print("\n", .{});
    return offset + 2;
}
