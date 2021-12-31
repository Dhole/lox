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
const U8_COUNT = _common.U8_COUNT;
const Obj = _object.Obj;
const ObjString = _object.ObjString;
const Objects = _object.Objects;
const ObjFunction = _object.ObjFunction;
const ObjType = _object.ObjType;
const allocate = _memory.allocate;
const Table = _table.Table;

pub const CallFrame = struct {
    function: *ObjFunction,
    pc: usize,
    // slots: []Value,
    slots: usize,
};

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const InterpretError = error{
    Compile,
    Runtime,
};

pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

pub fn VM(comptime flags: Flags) type {
    return struct {
        const Self = @This();

        frame: *CallFrame,
        frames: [FRAMES_MAX]CallFrame,
        frameCount: usize,
        stack: [STACK_MAX]Value,
        stackTop: usize,
        globals: Table,
        objects: Objects,

        pub fn init() Self {
            return Self{
                .frame = undefined,
                .frames = undefined,
                .stack = undefined,
                .stackTop = 0,
                .frameCount = 0,
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
            self.frameCount = 0;
        }

        pub fn push(self: *Self, value: Value) void {
            self.stack[self.stackTop] = value;
            self.stackTop += 1;
            std.log.debug("DBG push {d}", .{self.stackTop});
        }

        pub fn pop(self: *Self) Value {
            std.log.debug("DBG pop {d}", .{self.stackTop});
            self.stackTop -= 1;
            return self.stack[self.stackTop];
        }

        // pub fn ip(self: *Self) u8 {
        //     return self.chunk.code[self.pc];
        // }

        pub fn interpret(self: *Self, source: []const u8) InterpretResult {
            var parser = Parser(flags).init();

            const function = blk: {
                if (parser.compile(&self.objects, source)) |fun| {
                    break :blk fun;
                } else {
                    return InterpretResult.COMPILE_ERROR;
                }
            };
            self.push(.{ .obj = function.asObj() });
            if (!self.call(function, 0)) {
                @panic("script call");
            }

            self.run() catch |err| {
                switch (err) {
                    InterpretError.Runtime => return InterpretResult.RUNTIME_ERROR,
                    InterpretError.Compile => return InterpretResult.COMPILE_ERROR,
                }
            };
            return InterpretResult.OK;
        }

        fn run(self: *Self) InterpretError!void {
            self.frame = &self.frames[self.frameCount - 1];

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
                    _ = disassembleInstruction(&self.frame.function.chunk, self.frame.pc);
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
                        self.push(self.stack[self.frame.slots + slot]);
                    },
                    @enumToInt(OpCode.SET_LOCAL) => {
                        const slot = self.readByte();
                        self.stack[self.frame.slots + slot] = self.peek(0);
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
                    @enumToInt(OpCode.JUMP) => {
                        const offset = self.readShort();
                        self.frame.pc += offset;
                    },
                    @enumToInt(OpCode.JUMP_IF_FALSE) => {
                        const offset = self.readShort();
                        if (isFalsey(self.peek(0))) {
                            self.frame.pc += offset;
                        }
                    },
                    @enumToInt(OpCode.PRINT) => {
                        printValue(self.pop());
                        print("\n", .{});
                    },
                    @enumToInt(OpCode.LOOP) => {
                        const offset = self.readShort();
                        self.frame.pc -= offset;
                    },
                    @enumToInt(OpCode.CALL) => {
                        const argCount = self.readByte();
                        if (!self.callValue(self.peek(argCount), argCount)) {
                            return InterpretError.Runtime;
                        }
                        self.frame = &self.frames[self.frameCount - 1];
                    },
                    @enumToInt(OpCode.RETURN) => {
                        const result = self.pop();
                        self.frameCount -= 1;
                        if (self.frameCount == 0) {
                            _ = self.pop();
                            return;
                        }
                        self.stackTop = self.frame.slots;
                        self.push(result);
                        self.frame = &self.frames[self.frameCount - 1];
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
            const b = self.frame.function.chunk.code[self.frame.pc];
            self.frame.pc += 1;
            return b;
        }

        fn readConstant(self: *Self) Value {
            return self.frame.function.chunk.constants.values[self.readByte()];
        }

        fn readString(self: *Self) *ObjString {
            return self.readConstant().obj.asString();
        }

        fn readShort(self: *Self) u16 {
            self.frame.pc += 2;
            return @intCast(u16, self.frame.function.chunk.code[self.frame.pc - 2]) << 8 |
                @intCast(u16, self.frame.function.chunk.code[self.frame.pc - 1]);
        }

        fn peek(self: *Self, distance: usize) Value {
            return self.stack[self.stackTop - 1 - distance];
        }

        fn call(self: *Self, function: *ObjFunction, argCount: usize) bool {
            if (argCount != function.arity) {
                self.runtimeError("Expected {d} arguments but got {d}.", .{ function.arity, argCount });
                return false;
            }
            if (self.frameCount == FRAMES_MAX) {
                self.runtimeError("Stack overflow.", .{});
                return false;
            }

            var frame = &self.frames[self.frameCount];
            self.frameCount += 1;
            frame.function = function;
            frame.pc = 0;
            frame.slots = self.stackTop - argCount - 1; // self.stack[self.stackTop - argCount - 1 ..];
            return true;
        }

        fn callValue(self: *Self, callee: Value, argCount: u8) bool {
            switch (callee) {
                Value.obj => |obj| {
                    switch (obj.type) {
                        ObjType.function => return self.call(obj.asFunction(), argCount),
                        else => {}, // Non-callable object type.
                    }
                },
                else => {},
            }
            self.runtimeError("Can only call functions and classes.", .{});
            return false;
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

            var i = @intCast(isize, self.frameCount - 1);
            while (i >= 0) : (i -= 1) {
                const frame = &self.frames[@intCast(usize, i)];
                const instruction = frame.pc - 1;
                const line = frame.function.chunk.lines[instruction];
                if (frame.function.name) |name| {
                    std.log.err("[line {d}] in {s}()", .{ line, name.chars });
                } else {
                    std.log.err("[line {d}] in script", .{line});
                }
            }

            self.resetStack();
        }
    };
}
