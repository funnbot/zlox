const std = @import("std");
const Chunk = @import("chunk.zig");
const Op = Chunk.OpCode;
const Value = @import("value.zig").Value;
const assert = std.debug.assert;
const stdout = std.io.getStdOut().writer();

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    try stdout.print("== {} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

// zig fmt: off
pub fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    try stdout.print("{:0<4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        try stdout.writeAll("   | ");
    } else {
        try stdout.print("{:<4} ", .{chunk.lines.items[offset]});
    }

    const inst = chunk.code.items[offset];
    const opName = @intToEnum(Op, @intCast(@TagType(Op), inst));
    return switch (opName) {
        Op.Nil,
        Op.True, Op.False,
        Op.Pop,
        Op.Greater, Op.Less,
        Op.Add, Op.Subtract, Op.Multiply, Op.Divide,
        Op.Not,
        Op.Negate,
        Op.Equal,
        Op.Print,
        Op.Return
        => simpleInstruction(opName, offset),

        Op.Constant,
        Op.GetGlobal, Op.DefineGlobal, Op.SetGlobal 
        => constantInstruction(opName, chunk, offset),

        Op.GetLocal, Op.SetLocal,
        Op.GetUpvalue, Op.CloseUpvalue, Op.SetUpvalue,
        Op.Call,
        => byteInstruction(opName, chunk, offset),

        Op.Closure => {
            var cloneOffset = offset;
            cloneOffset += 1;
            var constant = chunk.code.items[cloneOffset];
            cloneOffset += 1;

            try stdout.print("{} {} {}\n",  .{opName, constant, chunk.constants.items[constant]});

            var func = chunk.constants.items[constant].obj.toChild(.Function);
            var i: i32 = 0;
            while (i < func.upvalueCount) : (i += 1) {
                var isLocal = chunk.code.items[cloneOffset];
                cloneOffset += 1;
                var index = chunk.code.items[cloneOffset];
                cloneOffset += 1;

                var tName = if (isLocal == 1) "local" else "upvalue";
                try stdout.print("{:0<4}    |                {} {}\n", .{cloneOffset - 2, tName, index});
            }

            return cloneOffset;
        },

        Op.Jump, Op.JumpIfFalse
        => jumpInstruction(opName, 1, chunk, offset),

        Op.Loop 
        => jumpInstruction(opName, -1, chunk, offset),

        else => {
            try stdout.print("Unknown opcode {}\n", .{opName});
            return offset + 1;
        },
    };
    
}
// zig fmt: on

fn simpleInstruction(op: Op, offset: usize) !usize {
    try stdout.print("{}\n", .{op});
    return offset + 1;
}

fn constantInstruction(op: Op, chunk: *Chunk, offset: usize) !usize {
    assert(chunk.code.items.len - offset > 1);
    var constant: u8 = chunk.code.items[offset + 1];
    try stdout.print("{} {} {}\n", .{ op, constant, chunk.constants.items[constant] });
    return offset + 2;
}

fn byteInstruction(op: Op, chunk: *Chunk, offset: usize) !usize {
    assert(chunk.code.items.len - offset > 1);
    var slot: u8 = chunk.code.items[offset + 1];
    try stdout.print("{} {}\n", .{ op, slot });
    return offset + 2;
}

fn jumpInstruction(op: Op, sign: i32, chunk: *Chunk, offset: usize) !usize {
    assert(chunk.code.items.len - offset > 2);
    var jump: u16 = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= @as(u16, chunk.code.items[offset + 2]);
    try stdout.print("{} {} -> {}\n", .{ op, offset, @intCast(i32, offset + 3) + sign * @intCast(i32, jump) });
    return offset + 3;
}
