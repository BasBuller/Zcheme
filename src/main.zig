const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const ObjectType = enum {
    fixnum,
};

const Object = union(ObjectType) {
    fixnum: i64,
};

fn makeFixnum(value: i64, allocator: Allocator) !*Object {
    var res = try allocator.create(Object);
    res.fixnum = value;
    return res;
}

// Parser

// REPL
fn trimWhitespace(slice: []const u8) []const u8 {
    return std.mem.trim(u8, slice, " ");
}

fn repl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [128]u8 = undefined;

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |value| {
            try stdout.print("Mirroring: {s}\n", .{trimWhitespace(value)});
        } else {
            try stdout.print("Failed, please try again.\n", .{});
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    _ = makeFixnum(9, alloc);

    try repl();
}
