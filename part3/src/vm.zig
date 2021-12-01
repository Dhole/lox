const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");
const _debug = @import("debug.zig");
const _compiler = @import("compiler.zig");

const print = std.debug.print;

const Chunk = _chunk.Chunk;
const OpCode = _common.OpCode;
const Value = _value.Value;
const printValue = _value.printValue;
const disassembleInstruction = _debug.disassembleInstruction;
const Parser = _compiler.Parser;
const Flags = _common.Flags;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const STACK_MAX: usize = 256;

pub fn VM(comptime flags: Flags) type {
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
            var chunk = Chunk.init();
            var parser = Parser(flags).init();
            defer chunk.deinit();
            if (!parser.compile(source, &chunk)) {
                return InterpretResult.COMPILE_ERROR;
            }
            self.chunk = &chunk;
            self.pc = 0;

            const result = self.run();
            return result;
        }

        fn run(self: *Self) InterpretResult {
            while (true) {
                if (flags.debugTraceExecution) {
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
                    @enumToInt(OpCode.CONSTANT) => {
                        const constant = self.readConstant();
                        self.push(constant);
                    },
                    @enumToInt(OpCode.NEGATE) => self.push(-self.pop()),
                    @enumToInt(OpCode.ADD) => self.binaryOp(add),
                    @enumToInt(OpCode.SUBTRACT) => self.binaryOp(sub),
                    @enumToInt(OpCode.MULTIPLY) => self.binaryOp(mul),
                    @enumToInt(OpCode.DIVIDE) => self.binaryOp(div),
                    @enumToInt(OpCode.RETURN) => {
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
