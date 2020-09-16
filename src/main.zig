const std = @import("std");
const vm = @import("vm.zig");
const Chunk = @import("chunk.zig");
const Scanner = @import("scanner.zig");
const Compiler = @import("compiler.zig");

const fs = std.fs;
const Allocator = std.mem.Allocator;
const allocator: *Allocator = std.heap.c_allocator;

const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

const MainError = error.TooEnoughArgs;

const obj = @import("object.zig");

pub fn main() !void {
    try stdout.writeAll(
        \\*-----------------------------------*
        \\|                RUN                |
        \\*-----------------------------------*
        \\
    );

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    vm.vm_state = vm.VM.init(allocator);
    defer vm.vm_state.deinit();

    try vm.vm_state.setup();

    switch (args.len) {
        1 => try repl(),
        2 => try runFile(args[1]),
        else => return error.TooEnoughArgs,
    }
}

fn repl() !void {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    while (true) {
        try stdout.writeAll(">> ");

        if (try stdin.readUntilDelimiterArrayList(&buf, '\n', 1024)) {
            vm.interpret(allocator, buf.items) catch break;
        } else |_| {
            try stdout.writeByte('\n');
            break;
        }
    }
}

fn runFile(path: []const u8) !void {
    var bytes = try fs.Dir.readFileAlloc(fs.cwd(), allocator, path, std.math.maxInt(usize));
    defer allocator.free(bytes);

    vm.interpret(allocator, bytes) catch |_| return;
}
