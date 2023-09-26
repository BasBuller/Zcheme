const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const ObjectType = enum {
    boolean,
    fixnum,
};

const Object = union(ObjectType) {
    fixnum: i64,
    boolean: bool,
};

fn makeFixnum(value: i64, allocator: Allocator) !*Object {
    var res = try allocator.create(Object);
    res.fixnum = value;
    return res;
}

fn makeBoolean(value: bool, allocator: Allocator) !*Object {
    var res = try allocator.create(Object);
    res.boolean = value;
    return res;
}

// Parser
const ParseError = error{
    InvalidInput,
};

fn isDelimiter(char: u8) bool {
    return (std.ascii.isWhitespace(char) or (char == '(') or (char == ')') or (char == '"') or (char == ';'));
}

fn read(chars: []u8, allocator: Allocator) !*Object {
    var varChars = eatWhitespace(chars);

    if (varChars[0] == '#') { // Boolean
        var boolval = switch (varChars[1]) {
            't' => true,
            'f' => false,
            else => return ParseError.InvalidInput,
        };
        return makeBoolean(boolval, allocator);
    } else if ((varChars[0] == '-') or std.ascii.isDigit(varChars[0])) { // Fixnum
        var idx: usize = 1;
        while ((idx < varChars.len) and std.ascii.isDigit(varChars[idx])) {
            idx += 1;
        }
        var num = try std.fmt.parseInt(i64, varChars[0..idx], 10);
        return makeFixnum(num, allocator);
    } else {
        return ParseError.InvalidInput;
    }
}

// Evaluate
fn eval(expr: *Object) *Object {
    return expr;
}

// Printing
fn write(obj: *Object, writer: std.fs.File.Writer) !void {
    switch (obj.*) {
        Object.fixnum => |value| try writer.print("{d}\n", .{value}),
        Object.boolean => |value| {
            var val: u8 = undefined;
            if (value) {
                val = 't';
            } else {
                val = 'f';
            }
            try writer.print("#{c}\n", .{val});
        },
    }
}

// REPL
fn eatWhitespace(slice: []const u8) []const u8 {
    return std.mem.trimLeft(u8, slice, " ");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer: [128]u8 = undefined;

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |input| {
            var value = try read(input, allocator);
            try write(value, stdout);
        } else {
            try stdout.print("Failed, please try again.\n", .{});
        }
    }
}
