const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");
const _debug = @import("debug.zig");
const _compiler = @import("compiler.zig");
const _object = @import("object.zig");
const _memory = @import("memory.zig");
const _table = @import("table.zig");

const print = std.debug.print;

const Chunk = _chunk.Chunk;
const OpCode = _common.OpCode;
const ValueType = _value.ValueType;
const Value = _value.Value;
const printValue = _value.printValue;
const disassembleInstruction = _debug.disassembleInstruction;
const Parser = _compiler.Parser;
const Flags = _common.Flags;
const Obj = _object.Obj;
const ObjString = _object.ObjString;
const Objects = _object.Objects;
const allocate = _memory.allocate;
const Table = _table.Table;

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
        globals: Table,
        objects: Objects,

        pub fn init() Self {
            return Self{
                .chunk = undefined,
                .pc = undefined,
                .stack = undefined,
                .stackTop = 0,
                .globals = Table.init(),
                .objects = Objects.init(),
            };
        }

        pub fn deinit(self: *Self) void {
            self.globals.deinit();
            self.objects.deinit();
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
            if (!parser.compile(&self.objects, source, &chunk)) {
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
                    @enumToInt(OpCode.NIL) => self.push(.nil),
                    @enumToInt(OpCode.TRUE) => self.push(.{ .boolean = true }),
                    @enumToInt(OpCode.FALSE) => self.push(.{ .boolean = false }),
                    @enumToInt(OpCode.POP) => {
                        _ = self.pop();
                    },
                    @enumToInt(OpCode.GET_LOCAL) => {
                        const slot = self.readByte();
                        self.push(self.stack[slot]);
                    },
                    @enumToInt(OpCode.SET_LOCAL) => {
                        const slot = self.readByte();
                        self.stack[slot] = self.peek(0);
                    },
                    @enumToInt(OpCode.GET_GLOBAL) => {
                        const name = self.readString();
                        var value: Value = undefined;
                        if (!self.globals.get(name, &value)) {
                            self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                            return InterpretError.Runtime;
                        }
                        self.push(value);
                    },
                    @enumToInt(OpCode.DEFINE_GLOBAL) => {
                        const name = self.readString();
                        _ = self.globals.set(name, self.peek(0));
                        _ = self.pop();
                    },
                    @enumToInt(OpCode.SET_GLOBAL) => {
                        const name = self.readString();
                        if (self.globals.set(name, self.peek(0))) {
                            _ = self.globals.delete(name);
                            self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                            return InterpretError.Runtime;
                        }
                    },
                    @enumToInt(OpCode.EQUAL) => {
                        const b = self.pop();
                        const a = self.pop();
                        self.push(.{ .boolean = a.equals(b) });
                    },
                    @enumToInt(OpCode.GREATER) => try self.binaryOp(f64, bool, Value.initBool, greater),
                    @enumToInt(OpCode.LESS) => try self.binaryOp(f64, bool, Value.initBool, less),
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
                    @enumToInt(OpCode.ADD) => {
                        const b = self.peek(0);
                        const a = self.peek(1);
                        if (a.isString() and b.isString()) {
                            self.concatenate();
                        } else if (a.isNumber() and b.isNumber()) {
                            _ = self.pop();
                            _ = self.pop();
                            self.push(.{ .number = a.number + b.number });
                        } else {
                            self.runtimeError("Operands must be two numbers or two strings.", .{});
                            return InterpretError.Runtime;
                        }
                    },
                    @enumToInt(OpCode.SUBTRACT) => try self.binaryOp(f64, f64, Value.initNumber, sub),
                    @enumToInt(OpCode.MULTIPLY) => try self.binaryOp(f64, f64, Value.initNumber, mul),
                    @enumToInt(OpCode.DIVIDE) => try self.binaryOp(f64, f64, Value.initNumber, div),
                    @enumToInt(OpCode.NOT) => self.push(.{ .boolean = isFalsey(self.pop()) }),
                    @enumToInt(OpCode.PRINT) => {
                        printValue(self.pop());
                        print("\n", .{});
                    },
                    @enumToInt(OpCode.RETURN) => {
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
        fn greater(a: f64, b: f64) bool {
            return a > b;
        }
        fn less(a: f64, b: f64) bool {
            return a < b;
        }

        fn binaryOp(
            self: *Self,
            comptime I: type,
            comptime O: type,
            valueInit: fn (v: O) Value,
            op: fn (a: I, b: I) O,
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

        fn readString(self: *Self) *ObjString {
            return self.readConstant().obj.asString();
        }

        fn peek(self: *Self, distance: usize) Value {
            return self.stack[self.stackTop - 1 - distance];
        }

        fn isFalsey(value: Value) bool {
            return switch (value) {
                Value.nil => true,
                Value.boolean => |boolean| !boolean,
                else => false,
            };
        }

        fn concatenate(self: *Self) void {
            const b = self.pop().obj.asString();
            const a = self.pop().obj.asString();

            const length = a.chars.len + b.chars.len;
            var chars = allocate(u8, length);
            std.mem.copy(u8, chars[0..a.chars.len], a.chars);
            std.mem.copy(u8, chars[a.chars.len..], b.chars);
            const res = self.objects.takeString(chars);
            self.push(.{ .obj = res.asObj() });
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
