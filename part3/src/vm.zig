const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");
const _debug = @import("debug.zig");
const _compiler = @import("compiler.zig");

const print = std.debug.print;

const Chunk = _chunk.Chunk;
const OpCode = _common.OpCode;
const ValueType = _value.ValueType;
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

pub const InterpretError = error{
    Compile,
    Runtime,
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

            self.run() catch |err| {
                switch (err) {
                    InterpretError.Runtime => return InterpretResult.RUNTIME_ERROR,
                    InterpretError.Compile => return InterpretResult.COMPILE_ERROR,
                }
            };
            return InterpretResult.OK;
        }

        fn run(self: *Self) InterpretError!void {
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
                    @enumToInt(OpCode.NEGATE) => {
                        switch (self.peek(0)) {
                            ValueType.number => |v| {
                                _ = self.pop();
                                self.push(.{ .number = -v });
                            },
                            else => {
                                self.runtimeError("Operand must be a number.", .{});
                                return InterpretError.Runtime;
                            },
                        }
                    },
                    @enumToInt(OpCode.ADD) => try self.binaryOp(f64, Value.initNumber, add),
                    @enumToInt(OpCode.SUBTRACT) => try self.binaryOp(f64, Value.initNumber, sub),
                    @enumToInt(OpCode.MULTIPLY) => try self.binaryOp(f64, Value.initNumber, mul),
                    @enumToInt(OpCode.DIVIDE) => try self.binaryOp(f64, Value.initNumber, div),
                    @enumToInt(OpCode.RETURN) => {
                        printValue(self.pop());
                        print("\n", .{});
                        return;
                    },
                    else => {
                        return InterpretError.Runtime;
                    },
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

        fn binaryOp(
            self: *Self,
            comptime T: type,
            valueInit: fn (v: T) Value,
            op: fn (a: T, b: T) T,
        ) InterpretError!void {
            const b = switch (self.peek(0)) {
                ValueType.number => |b| b,
                else => {
                    self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.Runtime;
                },
            };
            const a = switch (self.peek(1)) {
                ValueType.number => |a| a,
                else => {
                    self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.Runtime;
                },
            };
            _ = self.pop();
            _ = self.pop();
            self.push(valueInit(op(a, b)));
            return;
        }

        fn readByte(self: *Self) u8 {
            const b = self.ip();
            self.pc += 1;
            return b;
        }

        fn readConstant(self: *Self) Value {
            return self.chunk.constants.values[self.readByte()];
        }

        fn peek(self: *Self, distance: usize) Value {
            return self.stack[self.stackTop - 1 - distance];
        }

        fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
            std.log.err(fmt, args);
            const instruction = self.pc - 1;
            const line = self.chunk.lines[instruction];
            std.log.err("[line {d}] in script", .{line});
            self.resetStack();
        }
    };
}
