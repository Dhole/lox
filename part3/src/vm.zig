const std = @import("std");

const _chunk = @import("chunk.zig");
const _common = @import("common.zig");
const _value = @import("value.zig");
const _debug = @import("debug.zig");
const _compiler = @import("compiler.zig");
const _object = @import("object.zig");
const _memory = @import("memory.zig");
const _table = @import("table.zig");
const _global = @import("global.zig");

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
const ObjClosure = _object.ObjClosure;
const ObjUpvalue = _object.ObjUpvalue;
const ObjType = _object.ObjType;
const ObjNative = _object.ObjNative;
const ObjClass = _object.ObjClass;
const ObjInstance = _object.ObjInstance;
const NativeFn = _object.NativeFn;
const allocate = _memory.allocate;
const allocator = _memory.allocator;
const Table = _table.Table;
const debugLogGC = _memory.debugLogGC;

pub const CallFrame = struct {
    closure: *ObjClosure,
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
pub const GC_HEAP_GROW_FACTOR: usize = 2;

pub fn VM(comptime flags: Flags) type {
    return struct {
        const Self = @This();

        frame: *CallFrame,
        frames: [FRAMES_MAX]CallFrame,
        frameCount: usize,
        stack: [STACK_MAX]Value,
        stackTop: usize,
        globals: Table,
        openUpvalues: ?*ObjUpvalue,
        parser: ?*Parser(flags),

        bytesAllocated: usize,
        nextGC: usize,
        objects: Objects,
        grayCount: usize,
        // grayCapacity: usize, // grayCapacity is grayStack.len
        grayStack: []*Obj,

        pub fn init() Self {
            var self = Self{
                .frame = undefined,
                .frames = undefined,
                .stack = undefined,
                .stackTop = 0,
                .frameCount = 0,
                .globals = Table.init(),
                .openUpvalues = null,
                .parser = null,
                .bytesAllocated = 0,
                .nextGC = 1024 * 1024,
                .objects = Objects.init(),
                .grayCount = 0,
                .grayStack = &[_]*Obj{},
            };
            _global.setVM(&self);
            self.defineNative("clock", clockNative);
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.globals.deinit();
            self.objects.deinit();
            if (self.grayStack.len != 0) {
                allocator.free(self.grayStack);
            }
        }

        pub fn resetStack(self: *Self) void {
            self.stackTop = 0;
            self.frameCount = 0;
            self.openUpvalues = null;
        }

        pub fn push(self: *Self, value: Value) void {
            self.stack[self.stackTop] = value;
            self.stackTop += 1;
        }

        pub fn pop(self: *Self) Value {
            self.stackTop -= 1;
            return self.stack[self.stackTop];
        }

        // pub fn ip(self: *Self) u8 {
        //     return self.chunk.code[self.pc];
        // }

        pub fn interpret(self: *Self, source: []const u8) InterpretResult {
            var parser = Parser(flags).init();
            self.parser = &parser;

            const function = blk: {
                if (parser.compile(&self.objects, source)) |fun| {
                    break :blk fun;
                } else {
                    return InterpretResult.COMPILE_ERROR;
                }
            };
            self.push(.{ .obj = function.asObj() });
            const closure = ObjClosure.init(&self.objects, function);
            _ = self.pop();
            self.push(.{ .obj = closure.asObj() });
            if (!self.call(closure, 0)) {
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
                    _ = disassembleInstruction(&self.frame.closure.function.chunk, self.frame.pc);
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
                    @enumToInt(OpCode.GET_PROPERTY) => {
                        if (!self.peek(0).isInstance()) {
                            self.runtimeError("Only instances have properties.", .{});
                            return InterpretError.Runtime;
                        }

                        const instance = self.peek(0).obj.asInstance();
                        const name = self.readString();

                        var value: Value = undefined;
                        if (instance.fields.get(name, &value)) {
                            _ = self.pop(); // Instance.
                            self.push(value);
                        } else {
                            self.runtimeError("Undefined property '{s}'.", .{name.chars});
                            return InterpretError.Runtime;
                        }
                    },
                    @enumToInt(OpCode.SET_PROPERTY) => {
                        if (!self.peek(1).isInstance()) {
                            self.runtimeError("Only instances have properties.", .{});
                            return InterpretError.Runtime;
                        }

                        const instance = self.peek(1).obj.asInstance();
                        _ = instance.fields.set(self.readString(), self.peek(0));
                        const value = self.pop();
                        _ = self.pop();
                        self.push(value);
                    },
                    @enumToInt(OpCode.EQUAL) => {
                        const b = self.pop();
                        const a = self.pop();
                        self.push(.{ .boolean = a.equals(b) });
                    },
                    @enumToInt(OpCode.GET_UPVALUE) => {
                        const slot = self.readByte();
                        self.push(self.frame.closure.upvalues[slot].?.location.*);
                    },
                    @enumToInt(OpCode.SET_UPVALUE) => {
                        const slot = self.readByte();
                        self.frame.closure.upvalues[slot].?.location.* = self.peek(0);
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
                    @enumToInt(OpCode.CLOSURE) => {
                        const function = self.readConstant().obj.asFunction();
                        const closure = ObjClosure.init(&self.objects, function);
                        self.push(.{ .obj = closure.asObj() });

                        for (closure.upvalues) |*upvalue| {
                            const isLocal = self.readByte();
                            const index = self.readByte();
                            if (isLocal != 0) {
                                upvalue.* = self.captureUpvalue(&self.stack[self.frame.slots + index]);
                            } else {
                                upvalue.* = self.frame.closure.upvalues[index];
                            }
                        }
                    },
                    @enumToInt(OpCode.CLOSE_UPVALUE) => {
                        self.closeUpvalues(&self.stack[self.stackTop - 1]);
                        _ = self.pop();
                    },
                    @enumToInt(OpCode.RETURN) => {
                        const result = self.pop();
                        self.closeUpvalues(&self.stack[self.frame.slots]);
                        self.frameCount -= 1;
                        if (self.frameCount == 0) {
                            _ = self.pop();
                            return;
                        }
                        self.stackTop = self.frame.slots;
                        self.push(result);
                        self.frame = &self.frames[self.frameCount - 1];
                    },
                    @enumToInt(OpCode.CLASS) => {
                        self.push(.{ .obj = ObjClass.init(&self.objects, self.readString()).asObj() });
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
            const b = self.frame.closure.function.chunk.code[self.frame.pc];
            self.frame.pc += 1;
            return b;
        }

        fn readConstant(self: *Self) Value {
            return self.frame.closure.function.chunk.constants.values[self.readByte()];
        }

        fn readString(self: *Self) *ObjString {
            return self.readConstant().obj.asString();
        }

        fn readShort(self: *Self) u16 {
            self.frame.pc += 2;
            return @intCast(u16, self.frame.closure.function.chunk.code[self.frame.pc - 2]) << 8 |
                @intCast(u16, self.frame.closure.function.chunk.code[self.frame.pc - 1]);
        }

        fn peek(self: *Self, distance: usize) Value {
            return self.stack[self.stackTop - 1 - distance];
        }

        fn call(self: *Self, closure: *ObjClosure, argCount: usize) bool {
            if (argCount != closure.function.arity) {
                self.runtimeError("Expected {d} arguments but got {d}.", .{ closure.function.arity, argCount });
                return false;
            }
            if (self.frameCount == FRAMES_MAX) {
                self.runtimeError("Stack overflow.", .{});
                return false;
            }

            var frame = &self.frames[self.frameCount];
            self.frameCount += 1;
            frame.closure = closure;
            frame.pc = 0;
            frame.slots = self.stackTop - argCount - 1; // self.stack[self.stackTop - argCount - 1 ..];
            return true;
        }

        fn callValue(self: *Self, callee: Value, argCount: u8) bool {
            switch (callee) {
                Value.obj => |obj| {
                    switch (obj.type) {
                        ObjType.class => {
                            const klass = obj.asClass();
                            self.stack[self.stackTop - argCount - 1] =
                                .{ .obj = ObjInstance.init(&self.objects, klass).asObj() };
                            return true;
                        },
                        ObjType.closure => return self.call(obj.asClosure(), argCount),
                        ObjType.native => {
                            const native = obj.asNative();
                            const result = native.function(argCount, self.stack[self.stackTop - argCount ..]);
                            self.stackTop -= argCount + 1;
                            self.push(result);
                            return true;
                        },
                        else => {}, // Non-callable object type.
                    }
                },
                else => {},
            }
            self.runtimeError("Can only call functions and classes.", .{});
            return false;
        }

        fn captureUpvalue(self: *Self, local: *Value) *ObjUpvalue {
            var prevUpvalue: ?*ObjUpvalue = null;
            var upvalue = self.openUpvalues;
            while (upvalue) |upv| {
                if (@ptrToInt(upv.location) <= @ptrToInt(local)) {
                    break;
                }
                prevUpvalue = upv;
                upvalue = upv.next;
            }

            if (upvalue) |upv| {
                if (upv.location == local) {
                    return upv;
                }
            }

            var createdUpvalue = ObjUpvalue.init(&self.objects, local);
            createdUpvalue.next = upvalue;

            if (prevUpvalue) |prevUpv| {
                prevUpv.next = createdUpvalue;
            } else {
                self.openUpvalues = createdUpvalue;
            }

            return createdUpvalue;
        }

        fn closeUpvalues(self: *Self, last: *Value) void {
            while (self.openUpvalues) |openUpvs| {
                if (@ptrToInt(openUpvs.location) < @ptrToInt(last)) {
                    break;
                }
                var upvalue = openUpvs;
                upvalue.closed = upvalue.location.*;
                upvalue.location = &upvalue.closed;
                self.openUpvalues = upvalue.next;
            }
        }

        fn isFalsey(value: Value) bool {
            return switch (value) {
                Value.nil => true,
                Value.boolean => |boolean| !boolean,
                else => false,
            };
        }

        fn concatenate(self: *Self) void {
            const b = self.peek(0).obj.asString();
            const a = self.peek(1).obj.asString();

            const length = a.chars.len + b.chars.len;
            var chars = allocate(u8, length);
            std.mem.copy(u8, chars[0..a.chars.len], a.chars);
            std.mem.copy(u8, chars[a.chars.len..], b.chars);
            const res = self.objects.takeString(chars);
            _ = self.pop();
            _ = self.pop();
            self.push(.{ .obj = res.asObj() });
        }

        fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
            std.log.err(fmt, args);

            var i = @intCast(isize, self.frameCount - 1);
            while (i >= 0) : (i -= 1) {
                const frame = &self.frames[@intCast(usize, i)];
                const instruction = frame.pc - 1;
                const line = frame.closure.function.chunk.lines[instruction];
                if (frame.closure.function.name) |name| {
                    std.log.err("[line {d}] in {s}()", .{ line, name.chars });
                } else {
                    std.log.err("[line {d}] in script", .{line});
                }
            }

            self.resetStack();
        }

        fn defineNative(self: *Self, name: []const u8, function: NativeFn) void {
            self.push(.{ .obj = self.objects.copyString(name).asObj() });
            self.push(.{ .obj = ObjNative.init(&self.objects, function).asObj() });
            _ = self.globals.set(self.stack[0].obj.asString(), self.stack[1]);
            _ = self.pop();
            _ = self.pop();
        }

        pub fn collectGarbage(self: *Self) void {
            var before: usize = undefined;
            if (debugLogGC) {
                print("-- gc begin\n", .{});
                before = self.bytesAllocated;
                // if (_object.toggle) {
                //     @panic("begin");
                // }
            }
            self.markRoots();
            self.traceReferences();
            self.objects.strings.removeWhite();
            self.sweep();
            self.nextGC = self.bytesAllocated * GC_HEAP_GROW_FACTOR;
            if (debugLogGC) {
                print("-- gc end\n", .{});
                print("   collected {d} bytes (from {d} to {d}) next at {d}\n", //
                    .{ before - self.bytesAllocated, before, self.bytesAllocated, self.nextGC });
            }
        }

        fn markRoots(self: *Self) void {
            var i: usize = 0;
            while (i < self.stackTop) : (i += 1) {
                var slot = &self.stack[i];
                slot.mark();
            }
            i = 0;
            while (i < self.frameCount) : (i += 1) {
                self.frames[i].closure.asObj().mark();
            }
            var upvalue = self.openUpvalues;
            while (upvalue) |upv| {
                upv.asObj().mark();
                upvalue = upv.next;
            }
            // markTable(&vm.globals);
            self.globals.mark();
            if (self.parser) |parser| {
                parser.markCompilerRoots();
            }
        }

        fn traceReferences(self: *Self) void {
            while (self.grayCount > 0) {
                self.grayCount -= 1;
                var object = self.grayStack[self.grayCount];
                object.blacken();
            }
        }

        fn sweep(self: *Self) void {
            var previous: ?*Obj = null;
            var object: ?*Obj = self.objects.objects;
            var count: usize = 0;

            while (object) |obj| {
                count += 1;
                if (obj.isMarked) {
                    obj.isMarked = false;
                    previous = object;
                    object = obj.next;
                } else {
                    var unreached = obj;
                    object = obj.next;
                    if (previous) |prev| {
                        prev.next = object;
                    } else {
                        self.objects.objects = object;
                    }
                    unreached.deinit();
                }
            }
        }
    };
}

fn clockNative(argCount: usize, args: []Value) Value {
    _ = argCount;
    _ = args;
    return .{ .number = @intToFloat(f64, std.time.milliTimestamp()) / 1000.0 };
}
