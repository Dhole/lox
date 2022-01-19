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
        @enumToInt(OpCode.PRINT) => simpleInstruction("OP_PRINT", offset),
        @enumToInt(OpCode.JUMP) => jumpInstruction("OP_JUMP", 1, chunk, offset),
        @enumToInt(OpCode.JUMP_IF_FALSE) => jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        @enumToInt(OpCode.LOOP) => jumpInstruction("OP_LOOP", -1, chunk, offset),
        @enumToInt(OpCode.CALL) => byteInstruction("OP_CALL", chunk, offset),
        @enumToInt(OpCode.CLOSURE) => blk: {
            offset += 1;
            const constant = chunk.code[offset + 1];
            offset += 1;
            print("{s: <16} {d: >4} ", .{ "OP_CLOSURE", constant });
            printValue(chunk.constants.values[constant]);
            print("\n", .{});

            const function = chunk.constants.values[constant].obj.asFunction();
            var j: usize = 0;
            while (j < function.upvalueCount) : (j += 1) {
                const isLocal = chunk.code[offset];
                offset += 1;
                const index = chunk.code[offset];
                offset += 1;
                print("{d: >4}      |                     {s} {d}", //
                    .{ offset - 2, if (isLocal) "local" else "upvalue", index });
            }

            break :blk offset;
        },
        @enumToInt(OpCode.CLOSE_UPVALUE) => simpleInstruction("OP_CLOSE_UPVALUE", offset),
        @enumToInt(OpCode.RETURN) => simpleInstruction("OP_RETURN", offset),
        @enumToInt(OpCode.CLASS) => constantInstruction("OP_CLASS", chunk, offset),
        @enumToInt(OpCode.METHOD) => constantInstruction("OP_METHOD", chunk, offset),
        @enumToInt(OpCode.CONSTANT) => constantInstruction("OP_CONSTANT", chunk, offset),
        @enumToInt(OpCode.NIL) => simpleInstruction("OP_NIL", offset),
        @enumToInt(OpCode.TRUE) => simpleInstruction("OP_TRUE", offset),
        @enumToInt(OpCode.FALSE) => simpleInstruction("OP_FALSE", offset),
        @enumToInt(OpCode.POP) => simpleInstruction("OP_POP", offset),
        @enumToInt(OpCode.GET_LOCAL) => byteInstruction("OP_GET_LOCAL", chunk, offset),
        @enumToInt(OpCode.SET_LOCAL) => byteInstruction("OP_SET_LOCAL", chunk, offset),
        @enumToInt(OpCode.GET_GLOBAL) => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        @enumToInt(OpCode.DEFINE_GLOBAL) => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        @enumToInt(OpCode.SET_GLOBAL) => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        @enumToInt(OpCode.GET_UPVALUE) => byteInstruction("OP_GET_UPVALUE", chunk, offset),
        @enumToInt(OpCode.SET_UPVALUE) => byteInstruction("OP_SET_UPVALUE", chunk, offset),
        @enumToInt(OpCode.GET_PROPERTY) => constantInstruction("OP_GET_PROPERTY", chunk, offset),
        @enumToInt(OpCode.SET_PROPERTY) => constantInstruction("OP_SET_PROPERTY", chunk, offset),
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
    print("{s: <16} {d: >4} ", .{ name, constant });
    printValue(chunk.constants.values[constant]);
    print("\n", .{});
    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot = chunk.code[offset + 1];
    print("{s: <16} {d: >4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize) usize {
    const jump = @intCast(u16, chunk.code[offset + 1]) << 8 |
        @intCast(u16, chunk.code[offset + 2]);
    print("{s: <16} {d: >4} -> {d}\n", .{ name, offset, @intCast(isize, offset) + 3 + sign * @intCast(isize, jump) });
    return offset + 3;
}
