const std = @import("std");

pub const Allocator = std.mem.Allocator;
pub const allocator: *Allocator = std.heap.c_allocator;
