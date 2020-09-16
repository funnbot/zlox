const std = @import("std");
const vm = @import("vm.zig");

const Value = @import("value.zig").Value;
const Chunk = @import("chunk.zig");

const table = @import("table.zig");
const Table = table.Table;

const compiler = @import("compiler.zig");

const Allocator = std.mem.Allocator;
const TypeInfo = std.builtin.TypeInfo;
const assert = std.debug.assert;
const warn = std.debug.warn;

pub const ObjType = enum {
    String,
    Function,
    Native,
    Closure,
    Upvalue,

    pub fn toTypeName(self: ObjType) []const u8 {
        inline for (std.meta.fields(ObjType)) |field| {
            const typ = @intToEnum(ObjType, field.value);
            if (typ == self) return @typeName(typ.toType());
        }
        unreachable;
    }

    pub fn toType(comptime self: ObjType) type {
        return switch (self) {
            .String => ObjString,
            .Function => ObjFunction,
            .Native => ObjNative,
            .Closure => ObjClosure,
            .Upvalue => ObjUpvalue,
        };
    }
};

pub const Obj = struct {
    typ: ObjType,
    next: ?*Obj = null,

    isMarked: bool = false,

    allocator: *Allocator,

    pub fn init(self: *Obj, allocator: *Allocator, typ: ObjType) void {
        self.* = Obj{
            .typ = typ,
            .allocator = allocator,
        };
        vm.vm_state.listPrepend(self);

        //if (vm.GC_LOG)
        //warn("alloc {c}\n", .{self});
    }

    pub fn create(allocator: *Allocator, comptime T: type) !*T {
        vm.state.bytesAllocated += @sizeOf(T);

        if (vm.GC_STRESS)
            try collectGarbage();

        if (vm.state.bytesAllocated > vm.state.nextGC) {
            try collectGarbage();
        }

        var ptr = try allocator.create(T);

        return ptr;
    }

    pub fn destroyChild(self: *Obj) void {
        inline for (std.meta.fields(ObjType)) |field| {
            const typ = @intToEnum(ObjType, field.value);
            if (typ == self.typ) {
                var child = self.toChild(typ);

                //if (vm.GC_LOG)
                //warn("free  {c}\n", .{self});

                vm.state.bytesAllocated -= @sizeOf(typ.toType());

                child.destroy();
            }
        }
    }

    pub fn toChild(self: *const Obj, comptime ObjT: ObjType) *ObjType.toType(ObjT) {
        // unsafe
        return @intToPtr(*ObjT.toType(), @ptrToInt(@fieldParentPtr(ObjT.toType(), "obj", self)));
    }

    pub fn format(value: *const Obj, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        if (std.mem.eql(u8, fmt, "c")) {
            try out_stream.print("{}#{} ", .{ value.typ.toTypeName(), @ptrToInt(value) % 31 });
        }

        inline for (std.meta.fields(ObjType)) |field| {
            const typ = @intToEnum(ObjType, field.value);
            if (typ == value.typ) try value.toChild(typ).format(fmt, options, out_stream);
        }
    }
};

pub const ObjString = struct {
    obj: Obj = undefined,

    chars: []const u8,
    hash: u32,

    /// ObjString does not own chars
    pub fn constStr(chars: []const u8, hash: u32) ObjString {
        return ObjString{
            .obj = Obj{ .typ = .String, .next = null, .allocator = undefined },
            .chars = chars,
            .hash = hash,
        };
    }

    /// ObjString owns chars, will free with destroy.
    pub fn create(allocator: *Allocator, chars: []const u8, hash: u32) !*ObjString {
        var string = try Obj.create(allocator, ObjString);
        string.* = .{
            .chars = chars,
            .hash = hash,
        };
        string.obj.init(allocator, .String);

        const alreadSet = try vm.vm_state.strings.put(string, .{ .nil = 0 });
        assert(alreadSet == null);

        return string;
    }

    pub fn destroy(self: *ObjString) void {
        self.obj.allocator.free(self.chars);
        self.obj.allocator.destroy(self);
    }

    pub fn copy(allocator: *Allocator, chars: []const u8) !*ObjString {
        var hash = hashStr(chars);
        var interned = table.findString(&vm.vm_state.strings, chars, hash);
        if (interned != null) return interned.?;

        var heapChars = try allocator.dupe(u8, chars);

        return create(allocator, heapChars, hash);
    }

    /// Use same allocator that created chars
    pub fn take(allocator: *Allocator, chars: []u8) !*ObjString {
        var hash = hashStr(chars);
        var interned = table.findString(&vm.vm_state.strings, chars, hash);
        if (interned != null) {
            allocator.free(chars);
            return interned.?;
        }

        return create(allocator, chars, hash);
    }

    fn hashStr(chars: []const u8) u32 {
        var h: u32 = 2166136261;
        for (chars) |char| {
            h ^= char;
            h *%= 16777619;
        }
        return h;
    }

    pub fn format(value: *const ObjString, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try out_stream.print("\"{}\"", .{value.chars});
    }
};

pub const ObjFunction = struct {
    obj: Obj = undefined,

    arity: i32 = 0,
    chunk: Chunk,
    name: ?*ObjString,
    upvalueCount: i32 = 0,

    pub fn create(allocator: *Allocator) !*ObjFunction {
        var func = try Obj.create(allocator, ObjFunction);
        func.* = ObjFunction{
            .chunk = Chunk.init(allocator),
            .name = null,
        };
        func.obj.init(allocator, .Function);

        return func;
    }

    pub fn destroy(self: *ObjFunction) void {
        self.chunk.deinit();
        self.obj.allocator.destroy(self);
    }

    pub fn format(value: *const ObjFunction, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        if (value.name == null)
            try out_stream.writeAll("<script>")
        else
            try out_stream.print("<fn {}>", .{value.name.?.chars});
    }
};

pub const NativeFn = fn (argCount: u8, args: [*]Value) Value;
pub const ObjNative = struct {
    obj: Obj = undefined,

    func: NativeFn,

    pub fn create(allocator: *Allocator, func: NativeFn) !*ObjNative {
        var native = try Obj.create(allocator, ObjNative);
        native.* = ObjNative{
            .func = func,
        };
        native.obj.init(allocator, .Native);

        return native;
    }

    pub fn destroy(self: *ObjNative) void {
        self.obj.allocator.destroy(self);
    }

    pub fn format(value: *const ObjNative, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try out_stream.writeAll("<native fn>");
    }
};

pub const ObjClosure = struct {
    obj: Obj = undefined,

    func: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalueCount: i32 = 0,

    pub fn create(allocator: *Allocator, func: *ObjFunction) !*ObjClosure {
        var closure = try Obj.create(allocator, ObjClosure);
        closure.* = ObjClosure{
            .func = func,
            .upvalueCount = func.upvalueCount,
            .upvalues = try allocator.alloc(*ObjUpvalue, @intCast(usize, func.upvalueCount)),
        };
        closure.obj.init(allocator, .Closure);

        return closure;
    }

    pub fn destroy(self: *ObjClosure) void {
        self.obj.allocator.free(self.upvalues);
        self.obj.allocator.destroy(self);
    }

    pub fn format(value: *const ObjClosure, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try ObjFunction.format(value.func, fmt, options, out_stream);
    }
};

pub const ObjUpvalue = struct {
    obj: Obj = undefined,

    location: *Value,
    next: ?*ObjUpvalue = null,
    closed: Value = Value{ .nil = 0 },

    pub fn create(allocator: *Allocator, location: *Value) !*ObjUpvalue {
        var upvalue = try Obj.create(allocator, ObjUpvalue);
        upvalue.* = ObjUpvalue{
            .location = location,
        };
        upvalue.obj.init(allocator, .Upvalue);

        return upvalue;
    }

    pub fn destroy(self: *ObjUpvalue) void {
        self.obj.allocator.destroy(self);
    }

    pub fn format(value: *const ObjUpvalue, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        try out_stream.writeAll("upvalue");
    }
};

pub fn markObject(obj: ?*Obj) !void {
    if (obj == null) return;
    if (obj.?.isMarked) return;

    if (vm.GC_LOG)
        warn("- mark {c}\n", .{obj.?});

    obj.?.isMarked = true;
    try vm.state.grayStack.push(obj.?);
}

fn markTable(tbl: *Table) !void {
    var iter = tbl.iterator();
    var kv = iter.next();
    while (kv != null) : (kv = iter.next()) {
        try markObject(&kv.?.key.obj);
        try markValue(kv.?.value);
    }
}

fn markValue(value: Value) !void {
    if (!value.isType(.obj)) return;
    try markObject(value.obj);
}

fn markArray(array: *std.ArrayList(Value)) !void {
    for (array.items) |val| try markValue(val);
}

fn blackenObject(obj: *Obj) !void {
    if (vm.GC_LOG)
        warn("- blacken {c}\n", .{obj});

    switch (obj.typ) {
        .Closure => {
            var closure: *ObjClosure = obj.toChild(.Closure);
            try markObject(&closure.func.obj);
            var i: usize = 0;
            while (i < closure.upvalueCount) : (i += 1) {
                try markObject(&closure.upvalues[i].obj);
            }
        },
        .Function => {
            var func = obj.toChild(.Function);
            if (func.name != null) try markObject(&func.name.?.obj);
            try markArray(&func.chunk.constants);
        },
        .Upvalue => try markValue(obj.toChild(.Upvalue).closed),
        .Native, .String => {},
    }
}

fn markRoots() !void {
    var slot: [*]Value = &vm.state.stack;
    while (@ptrToInt(slot) < @ptrToInt(vm.state.stackTop)) : (slot += 1) {
        try markValue(slot[0]);
    }

    var i: usize = 0;
    while (i < vm.state.frameCount) : (i += 1) {
        try markObject(&vm.state.frames[i].closure.obj);
    }

    var upvalue = vm.state.openUpvalues;
    while (upvalue != null) : (upvalue = upvalue.?.next) {
        try markObject(&upvalue.?.obj);
    }

    try markTable(&vm.state.global);
    try compiler.markCompilerRoots();
}

fn traceReferences() !void {
    while (vm.state.grayStack.size > 0) {
        var obj = try vm.state.grayStack.pop();
        try blackenObject(obj);
    }
}

fn tableRemoveWhite(tbl: *Table) void {
    for (tbl.items()) |entry| {
        if (!entry.used) continue;
        if (entry.kv.key.obj.isMarked) continue;
        _ = tbl.remove(entry.kv.key);
    }
}

fn sweep() void {
    var previous: ?*Obj = null;
    var object: ?*Obj = vm.state.objects;
    while (object != null) {
        if (object.?.isMarked) {
            object.?.isMarked = false;
            previous = object;
            object = object.?.next;
        } else {
            var unreached = object;

            object = object.?.next;
            if (previous != null) {
                previous.?.next = object;
            } else {
                vm.state.objects = object;
            }

            unreached.?.destroyChild();
        }
    }
}

pub fn collectGarbage() !void {
    var before: usize = undefined;
    if (vm.GC_LOG) {
        warn("-> gc begin\n", .{});
        before = vm.state.bytesAllocated;
    }

    try markRoots();
    try traceReferences();
    tableRemoveWhite(&vm.state.strings);
    sweep();

    vm.state.nextGC += vm.state.bytesAllocated * vm.GC_HEAP_GROW_FAC;

    if (vm.GC_LOG) {
        warn("-< gc end: collected {} bytes ({} to {}) next {}\n", .{ before - vm.state.bytesAllocated, before, vm.state.bytesAllocated, vm.state.nextGC });
    }
}
