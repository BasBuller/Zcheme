const std = @import("std");

fn trimWhitespace(slice: []const u8) []const u8 {
    return std.mem.trim(u8, slice, " ");
}

fn repl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [128]u8 = undefined;

    try stdout.print("\n", .{});
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
    try repl();
}
