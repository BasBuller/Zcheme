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
const ParseError = error{
    InvalidInput,
};

fn isDelimiter(char: u8) bool {
    return (std.ascii.isWhitespace(char) or (char == '(') or (char == ')') or (char == '"') or (char == ';'));
}

fn read(chars: []u8) !Object {
    var varChars = eatWhitespace(chars);

    if ((varChars[0] == '-') or std.ascii.isDigit(varChars[0])) {
        var idx: usize = 1;
        while ((idx < varChars.len) and std.ascii.isDigit(varChars[idx])) {
            idx += 1;
        }
        var num = try std.fmt.parseInt(i64, varChars[0..idx], 10);
        return Object{ .fixnum = num };
    } else {
        return ParseError.InvalidInput;
    }
}

// REPL
fn eatWhitespace(slice: []const u8) []const u8 {
    return std.mem.trimLeft(u8, slice, " ");
}

fn repl() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [128]u8 = undefined;

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |input| {
            var value = read(input);
            try stdout.print("{any}\n", .{value});
        } else {
            try stdout.print("Failed, please try again.\n", .{});
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    _ = try makeFixnum(9, alloc);

    try repl();
}
