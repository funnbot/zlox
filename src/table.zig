const std = @import("std");
const Value = @import("value.zig").Value;
const ObjString = @import("object.zig").ObjString;
const vm = @import("vm.zig");

pub const Table = std.HashMap(*ObjString, Value, hashFn, eqlFn, false);

fn hashFn(key: *ObjString) u32 {
    return key.hash;
}

fn eqlFn(a: *ObjString, b: *ObjString) bool {
    return std.mem.eql(u8, a.chars, b.chars);
}

pub fn findString(self: *Table, str: []const u8, hash: u32) ?*ObjString {
    if (self.items().len == 0) return null;
    var constStr = ObjString.constStr(str, hash);
    const result = vm.vm_state.strings.get(&constStr);
    return if (result) |kv| kv.key else null;
}
