const std = @import("std");

const chunk = @import("chunk.zig");
const common = @import("common.zig");
const value = @import("value.zig");

const print = std.debug.print;
const Chunk = chunk.Chunk;
const OpCode = common.OpCode;
const printValue = value.printValue;
// const print = std.log.info;

pub fn disassembleChunk(ch: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < ch.count) {
        offset = disassembleInstruction(ch, offset);
    }
}

pub fn disassembleInstruction(ch: *Chunk, offset: usize) usize {
    print("{x:0>4} ", .{offset});
    if (offset > 0 and ch.lines[offset] == ch.lines[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:0>4} ", .{ch.lines[offset]});
    }

    const instruction = ch.code[offset];
    return switch (instruction) {
        @enumToInt(OpCode.OP_RETURN) => simpleInstruction("OP_RETURN", offset),
        @enumToInt(OpCode.OP_CONSTANT) => constantInstruction("OP_CONSTANT", ch, offset),
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

fn constantInstruction(name: []const u8, ch: *Chunk, offset: usize) usize {
    const constant = ch.code[offset + 1];
    print("{s} {d: >4} ", .{ name, constant });
    printValue(ch.constants.values[constant]);
    print("\n", .{});
    return offset + 2;
}
