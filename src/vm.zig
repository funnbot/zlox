const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig");
const Op = Chunk.OpCode;

const Stack = @import("stack.zig").Stack;

const obj = @import("object.zig");
const Obj = obj.Obj;

const Value = @import("value.zig").Value;
const Table = @import("table.zig").Table;

const Compiler = @import("compiler.zig");

const debug = @import("debug.zig");

pub const InterpretError = error{ Compile, Runtime };

const is_exec = !std.builtin.is_test;
pub const DEBUG_PRINT = is_exec and false;
pub const DEBUG_TRACE = is_exec and false;
pub const GC_STRESS = is_exec and false;
pub const GC_LOG = is_exec and true;

pub const GC_HEAP_GROW_FAC = 2;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * (std.math.maxInt(u8) + 1);

pub var vm_state: VM = undefined;
pub var state: *VM = &vm_state;

fn clockNative(argCount: u8, args: [*]Value) Value {
    return Value{ .number = @intToFloat(f64, vm_state.timer.read()) };
}

const CallFrame = struct {
    closure: *obj.ObjClosure,
    ip: [*]const u8,
    slots: [*]Value = undefined,

    pub fn init(closure: *obj.ObjClosure) CallFrame {
        return CallFrame{
            .closure = closure,
            .ip = closure.func.chunk.code.items.ptr,
        };
    }
};

pub const VM = struct {
    const Self = @This();

    frames: [FRAMES_MAX]CallFrame = undefined,
    frameCount: usize = 0,

    stack: [STACK_MAX]Value = undefined,
    stackTop: [*]Value = undefined,

    global: Table,
    strings: Table,

    objects: ?*Obj = null,
    openUpvalues: ?*obj.ObjUpvalue = null,

    grayStack: Stack(*Obj),

    bytesAllocated: usize = 0,
    nextGC: usize = 1024,

    allocator: *Allocator,
    timer: std.time.Timer,

    pub fn init(allocator: *Allocator) Self {
        return Self{
            .global = Table.init(allocator),
            .strings = Table.init(allocator),
            .grayStack = Stack(*Obj).init(allocator),
            .allocator = allocator,
            .timer = undefined,
        };
    }

    pub fn setup(self: *Self) !void {
        self.resetStack();
        self.timer = try std.time.Timer.start();

        try defineNative("clock", clockNative);
    }

    pub fn deinit(self: *Self) void {
        self.global.deinit();
        self.strings.deinit();
        self.grayStack.deinit();
        self.listDestroy();
    }

    pub fn setChunk(self: *Self, chunk: *Chunk) void {
        self.chunk = chunk;
        self.ip = chunk.code.items.ptr;
    }

    pub fn listPrepend(self: *Self, objPtr: *Obj) void {
        objPtr.next = self.objects;
        self.objects = objPtr;
    }

    pub fn listDestroy(self: *Self) void {
        if (self.objects == null) return;
        var temp: ?*Obj = self.objects.?;
        while (temp != null) {
            temp.?.destroyChild();
            temp = temp.?.next;
        }
    }

    pub fn resetStack(self: *Self) void {
        self.stackTop = &self.stack;
        self.frameCount = 0;
    }
};

fn ipIndex(frame: *CallFrame) usize {
    return @ptrToInt(frame.ip) - @ptrToInt(frame.closure.func.chunk.code.items.ptr);
}

fn runtimeError(comptime msg: []const u8, args: anytype) !void {
    try stderr.print(msg, args);
    try stderr.writeAll("\n");

    var i = vm_state.frameCount - 1;
    while (i >= 0) : (i -= 1) {
        var frame: *CallFrame = &vm_state.frames[i];
        var func: *obj.ObjFunction = frame.closure.func;

        var inst = (@ptrToInt(frame.ip) - @ptrToInt(func.chunk.code.items.ptr)) - 1;
        try stderr.print("[line {}] in ", .{func.chunk.lines.items[inst]});
        if (func.name == null)
            try stderr.writeAll("script \n")
        else
            try stderr.print("{}()\n", .{func.name.?.chars});

        if (i == 0) break;
    }

    vm_state.resetStack();
}

fn defineNative(name: []const u8, func: obj.NativeFn) !void {
    var str = try obj.ObjString.copy(vm_state.allocator, name);
    push(Value.fromObj(&str.obj));
    var native = try obj.ObjNative.create(vm_state.allocator, func);
    push(Value.fromObj(&native.obj));
    _ = try vm_state.global.put(vm_state.stack[0].obj.toChild(.String), vm_state.stack[1]);
    _ = pop();
    _ = pop();
}

pub fn push(value: Value) void {
    vm_state.stackTop[0] = value;
    vm_state.stackTop += 1;
}

pub fn pop() Value {
    vm_state.stackTop -= 1;
    return vm_state.stackTop[0];
}

fn peek(dist: usize) Value {
    return (vm_state.stackTop - (dist + 1))[0];
}

fn call(clos: *obj.ObjClosure, argCount: u8) !bool {
    if (argCount != clos.func.arity) {
        try runtimeError("Expected {} arguments but got {}.", .{ clos.func.arity, argCount });
        return false;
    }

    if (vm_state.frameCount == FRAMES_MAX) {
        try runtimeError("Stack overflow.", .{});
        return false;
    }

    var frame: *CallFrame = &vm_state.frames[vm_state.frameCount];
    vm_state.frameCount += 1;

    frame.* = CallFrame.init(clos);
    frame.slots = vm_state.stackTop - argCount - 1;

    return true;
}

fn callValue(callee: Value, argCount: u8) !bool {
    if (callee.isType(.obj)) {
        switch (callee.obj.typ) {
            .Closure => return try call(callee.obj.toChild(.Closure), argCount),
            .Native => {
                var native: *obj.ObjNative = callee.obj.toChild(.Native);
                var result = native.func(argCount, vm_state.stackTop - argCount);
                vm_state.stackTop -= argCount + 1;
                push(result);
                return true;
            },
            else => {},
        }
    }
    try runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn captureUpvalue(local: *Value) !*obj.ObjUpvalue {
    var prevUpvalue: ?*obj.ObjUpvalue = null;
    var upvalue: ?*obj.ObjUpvalue = vm_state.openUpvalues;

    while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local)) {
        prevUpvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and upvalue.?.location == local) return upvalue.?;

    var createdUpvalue = try obj.ObjUpvalue.create(vm_state.allocator, local);
    createdUpvalue.next = upvalue;

    if (prevUpvalue == null) {
        vm_state.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue.?.next = createdUpvalue;
    }

    return createdUpvalue;
}

fn closeUpvalues(last: *Value) void {
    while (vm_state.openUpvalues != null and @ptrToInt(vm_state.openUpvalues.?.location) >= @ptrToInt(last)) {
        var upvalue = vm_state.openUpvalues;
        upvalue.?.closed = upvalue.?.location.*;
        upvalue.?.location = &upvalue.?.closed;
        vm_state.openUpvalues = upvalue.?.next;
    }
}

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .boolean => |val| !val,
        else => false,
    };
}

fn concatenate() !void {
    var b: *obj.ObjString = pop().obj.toChild(.String);
    var a: *obj.ObjString = pop().obj.toChild(.String);

    var length = b.chars.len + a.chars.len;
    var chars = try vm_state.allocator.alloc(u8, length);
    std.mem.copy(u8, chars[0..a.chars.len], a.chars);
    std.mem.copy(u8, chars[a.chars.len .. a.chars.len + b.chars.len], b.chars);

    var str = try obj.ObjString.take(vm_state.allocator, chars);

    push(Value.fromObj(&str.obj));
}

fn readByte(frame: *CallFrame) u8 {
    frame.ip += 1;
    return (frame.ip - 1)[0];
}

fn readShort(frame: *CallFrame) u16 {
    frame.ip += 2;
    return (@as(u16, (frame.ip - 2)[0]) << 8) | @as(u16, (frame.ip - 1)[0]);
}

fn readConstant(frame: *CallFrame) Value {
    return frame.closure.func.chunk.constants.items[@as(usize, readByte(frame))];
}

fn readString(frame: *CallFrame) *obj.ObjString {
    return readConstant(frame).obj.toChild(.String);
}

const BinOp = enum { Add, Subtract, Multiply, Divide, Greater, Less };
fn binaryOp(op: BinOp) !void {
    if (peek(0) != .number or peek(1) != .number) {
        try runtimeError("Operands must be numbers.", .{});
        return InterpretError.Runtime;
    }
    var b = pop().number;
    var a = pop().number;
    switch (op) {
        .Add => push(Value{ .number = a + b }),
        .Subtract => push(Value{ .number = a - b }),
        .Multiply => push(Value{ .number = a * b }),
        .Divide => push(Value{ .number = a / b }),
        .Greater => push(Value{ .boolean = a > b }),
        .Less => push(Value{ .boolean = a < b }),
    }
}

fn execute() !void {
    var frame: *CallFrame = &vm_state.frames[vm_state.frameCount - 1];

    while (true) {
        if (DEBUG_TRACE) {
            try stdout.writeAll("           ");
            var slot: [*]Value = &vm_state.stack;
            while (@ptrToInt(slot) < @ptrToInt(vm_state.stackTop)) : (slot += 1) {
                try stdout.print("[ {} ]", .{slot[0]});
            }
            try stdout.writeAll("\n");
            _ = try debug.disassembleInstruction(&frame.closure.func.chunk, ipIndex(frame));
        }

        var inst = readByte(frame);
        if (inst > std.math.maxInt(@TagType(Op))) {
            try runtimeError("Invalid opcode.", .{});
            return InterpretError.Runtime;
        }
        var op = @intToEnum(Op, @intCast(@TagType(Op), inst));

        switch (op) {
            Op.Constant => {
                var constant = readConstant(frame);
                push(constant);
            },
            Op.Nil => push(Value{ .nil = 0 }),
            Op.True => push(Value{ .boolean = true }),
            Op.False => push(Value{ .boolean = false }),
            Op.Pop => _ = pop(),
            Op.GetLocal => {
                var slot = readByte(frame);
                push(frame.slots[slot]);
            },
            Op.SetLocal => {
                var slot = readByte(frame);
                frame.slots[@as(usize, slot)] = peek(0);
            },
            Op.GetUpvalue => {
                var slot = readByte(frame);
                push(frame.closure.upvalues[@as(usize, slot)].location.*);
            },
            Op.CloseUpvalue => {
                closeUpvalues(&(vm_state.stackTop - 1)[0]);
                _ = pop();
            },
            Op.SetUpvalue => {
                var slot = readByte(frame);
                frame.closure.upvalues[@as(usize, slot)].location.* = peek(0);
            },
            Op.GetGlobal => {
                var name = readString(frame);
                var res = vm_state.global.get(name);
                if (res == null) {
                    try runtimeError("Undefined variable '{}'.", .{name.chars});
                    return InterpretError.Runtime;
                }
                push(res.?.value);
            },
            Op.DefineGlobal => {
                var name = readString(frame);
                _ = try vm_state.global.put(name, peek(0));
                _ = pop();
            },
            Op.SetGlobal => {
                var name = readString(frame);
                if (!vm_state.global.contains(name)) {
                    try runtimeError("Undefined variable '{}'.", .{name.chars});
                    return InterpretError.Runtime;
                }
                _ = try vm_state.global.put(name, peek(0));
            },
            Op.Equal => {
                var b = pop();
                var a = pop();
                push(Value{ .boolean = Value.equal(a, b) });
            },
            Op.Greater => try binaryOp(.Greater),
            Op.Less => try binaryOp(.Less),
            Op.Add => {
                if (peek(0).isObjType(.String) and peek(1).isObjType(.String)) {
                    try concatenate();
                } else if (peek(0).isType(.number) and peek(1).isType(.number)) {
                    var a = pop().number;
                    var b = pop().number;
                    push(Value{ .number = a + b });
                } else {
                    try runtimeError("Operands must be two numbers or two strings.", .{});
                    return InterpretError.Runtime;
                }
            },
            Op.Subtract => try binaryOp(.Subtract),
            Op.Multiply => try binaryOp(.Multiply),
            Op.Divide => try binaryOp(.Divide),
            Op.Not => push(Value{ .boolean = isFalsey(pop()) }),
            Op.Negate => {
                if (peek(0).tagType() == .number) {
                    try runtimeError("Operands must be a number", .{});
                    return InterpretError.Runtime;
                }
                push(Value{ .number = -pop().number });
            },
            Op.Print => {
                try stdout.print("{}\n", .{pop()});
            },
            Op.Jump => {
                var offset = readShort(frame);
                frame.ip += offset;
            },
            Op.JumpIfFalse => {
                var offset = readShort(frame);
                if (isFalsey(peek(0))) frame.ip += offset;
            },
            Op.Loop => {
                var offset = readShort(frame);
                frame.ip -= offset;
            },
            Op.Call => {
                var argCount = readByte(frame);
                if (!try callValue(peek(argCount), argCount)) {
                    return InterpretError.Runtime;
                }
                frame = &vm_state.frames[vm_state.frameCount - 1];
            },
            Op.Closure => {
                var func = readConstant(frame).obj.toChild(.Function);
                var closure = try obj.ObjClosure.create(vm_state.allocator, func);
                push(Value.fromObj(&closure.obj));

                var i: usize = 0;
                while (i < closure.upvalueCount) : (i += 1) {
                    var isLocal = readByte(frame);
                    var index = readByte(frame);
                    if (isLocal == 1)
                        closure.upvalues[i] = try captureUpvalue(&(frame.slots + index)[0])
                    else
                        closure.upvalues[i] = frame.closure.upvalues[index];
                }
            },
            Op.Return => {
                var result = pop();

                closeUpvalues(&frame.slots[0]);

                vm_state.frameCount -= 1;
                if (vm_state.frameCount == 0) {
                    _ = pop();
                    return;
                }

                vm_state.stackTop = frame.slots;
                push(result);

                frame = &vm_state.frames[vm_state.frameCount - 1];
            },
        }
    }
}

pub fn interpret(allocator: *Allocator, source: []const u8) InterpretError!void {
    var func = Compiler.compile(allocator, source) catch return InterpretError.Compile;

    push(Value.fromObj(&func.obj));
    var closure = obj.ObjClosure.create(allocator, func) catch return InterpretError.Compile;
    _ = pop();

    push(Value.fromObj(&closure.obj));
    _ = callValue(Value.fromObj(&closure.obj), 0) catch return InterpretError.Compile;

    execute() catch return InterpretError.Runtime;
}

test "interpret" {
    vm_state = VM.init(std.testing.allocator);
    defer vm_state.deinit();

    try vm_state.setup();

    try interpret(std.testing.allocator, "var i = 10;");
}
