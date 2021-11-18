const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");
const _debug = @import("debug.zig");

const print = std.debug.print;

const Chunk = _chunk.Chunk;
const OpCode = _common.OpCode;
const Value = _value.Value;
const printValue = _value.printValue;
const disassembleInstruction = _debug.disassembleInstruction;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const STACK_MAX: usize = 256;

pub fn VM(comptime debug: bool) type {
    return struct {
        const Self = @This();

        chunk: *Chunk,
        pc: usize,
        stack: [STACK_MAX]Value,
        stackTop: usize,

        pub fn init() Self {
            return Self{
                .chunk = undefined,
                .pc = undefined,
                .stack = undefined,
                .stackTop = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            _ = self;
        }

        pub fn resetStack(self: *Self) void {
            self.stackTop = 0;
        }

        pub fn push(self: *Self, value: Value) void {
            self.stack[self.stackTop] = value;
            self.stackTop += 1;
        }

        pub fn pop(self: *Self) Value {
            self.stackTop -= 1;
            return self.stack[self.stackTop];
        }

        pub fn ip(self: *Self) u8 {
            return self.chunk.code[self.pc];
        }

        pub fn interpret(self: *Self, source: []const u8) InterpretResult {
            self.compile(source);
            return INTERPRET_OK;
        }

        fn run(self: *Self) InterpretResult {
            while (true) {
                if (debug) {
                    print("          ", .{});
                    var i: usize = 0;
                    while (i < self.stackTop) : (i += 1) {
                        print("[ ", .{});
                        printValue(self.stack[i]);
                        print(" ]", .{});
                    }
                    print("\n", .{});
                    _ = disassembleInstruction(self.chunk, self.pc);
                }

                const instruction = self.readByte();
                switch (instruction) {
                    @enumToInt(OpCode.OP_CONSTANT) => {
                        const constant = self.readConstant();
                        self.push(constant);
                    },
                    @enumToInt(OpCode.OP_NEGATE) => self.push(-self.pop()),
                    @enumToInt(OpCode.OP_ADD) => self.binaryOp(add),
                    @enumToInt(OpCode.OP_SUBTRACT) => self.binaryOp(sub),
                    @enumToInt(OpCode.OP_MULTIPLY) => self.binaryOp(mul),
                    @enumToInt(OpCode.OP_DIVIDE) => self.binaryOp(div),
                    @enumToInt(OpCode.OP_RETURN) => {
                        printValue(self.pop());
                        print("\n", .{});
                        return InterpretResult.OK;
                    },
                    else => {},
                }
            }
        }

        fn add(a: f64, b: f64) f64 {
            return a + b;
        }
        fn sub(a: f64, b: f64) f64 {
            return a - b;
        }
        fn mul(a: f64, b: f64) f64 {
            return a * b;
        }
        fn div(a: f64, b: f64) f64 {
            return a / b;
        }

        fn binaryOp(self: *Self, op: fn (a: f64, b: f64) f64) void {
            const b = self.pop();
            const a = self.pop();
            self.push(op(a, b));
        }

        fn readByte(self: *Self) u8 {
            const b = self.ip();
            self.pc += 1;
            return b;
        }

        fn readConstant(self: *Self) Value {
            return self.chunk.constants.values[self.readByte()];
        }
    };
}
