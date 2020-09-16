const std = @import("std");
const meta = std.meta;
const object = @import("object.zig");
const Obj = object.Obj;
const ObjType = object.ObjType;

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    obj: *Obj,
    nil: u0,

    pub fn format(value: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: anytype) !void {
        switch (value) {
            .boolean => |val| try std.fmt.formatType(val, fmt, options, out_stream, std.fmt.default_max_depth),
            .number => |val| try std.fmt.formatType(val, "d", options, out_stream, std.fmt.default_max_depth),
            .obj => |val| try std.fmt.formatType(val, fmt, options, out_stream, std.fmt.default_max_depth),
            .nil => try out_stream.writeAll("nil"),
        }
    }

    // needed: bus error when initializing in anon struct literal.
    pub fn fromObj(obj: *Obj) Value {
        return Value{ .obj = obj };
    }

    pub fn tagType(self: Value) @TagType(Value) {
        return @as(@TagType(Value), self);
    }

    pub fn isType(self: Value, typ: @TagType(Value)) bool {
        return self.tagType() == typ;
    }

    pub fn isObjType(self: Value, typ: ObjType) bool {
        if (!self.isType(.obj)) return false;
        return self.obj.typ == typ;
    }

    pub fn equal(a: Value, b: Value) bool {
        if (a.tagType() != b.tagType()) return false;

        return switch (a) {
            .boolean => |val| val == b.boolean,
            .number => |val| val == b.number,
            .obj => |val| val == b.obj,
            .nil => true,
        };
    }
};
