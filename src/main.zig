const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const Object = union(enum) {
    fixnum: i64,
    boolean: bool,
    character: u8,
};

// Parser
const ParseError = error{
    InvalidInput,
};

fn isDelimiter(char: u8) bool {
    return (std.ascii.isWhitespace(char) or (char == '(') or (char == ')') or (char == '"') or (char == ';'));
}

fn eatWhitespace(slice: []u8) []u8 {
    var idx: usize = 0;
    while ((idx < slice.len) and (std.ascii.isWhitespace(slice[idx]))) {
        idx += 1;
    }
    return slice[idx..];
}

fn readCharacter(chars: []u8) !u8 {
    var char = switch (chars[0]) {
        's' => {
            switch (chars[1]) {
                'p' => ' ',
                else => ParseError.InvalidInput,
            }
        },
        'n' => {
            switch (chars[1]) {
                'e' => '\n',
                else => ParseError.InvalidInput,
            }
        },
        else => ParseError.InvalidInput,
    };
    return char;
}

fn readFixnum(chars: []u8) !i64 {
    var idx: usize = 1;
    while ((idx < chars.len) and std.ascii.isDigit(chars[idx])) {
        idx += 1;
    }
    var num = try std.fmt.parseInt(i64, chars[0..idx], 10);
    return num;
}

fn read(chars: []u8, allocator: Allocator) !*Object {
    var varChars = eatWhitespace(chars);
    var object = try allocator.create(Object);

    if (varChars[0] == '#') { // Boolean or character
        switch (varChars[1]) {
            't' => object.* = .{ .boolean = true },
            'f' => object.* = .{ .boolean = false },
            // '\\' => object.*= .{ .character = try readCharacter(varChars[2..]) },
            else => return ParseError.InvalidInput,
        }
    } else if ((varChars[0] == '-') or std.ascii.isDigit(varChars[0])) { // Fixnum
        object.* = .{ .fixnum = try readFixnum(varChars) };
    } else {
        return ParseError.InvalidInput;
    }

    return object;
}

// Evaluate
fn eval(expr: *Object) *Object {
    return expr;
}

// Printing
fn write(obj: *Object, writer: std.fs.File.Writer) !void {
    switch (obj.*) {
        Object.fixnum => |value| try writer.print("{d}", .{value}),
        Object.boolean => |value| {
            var val: u8 = undefined;
            if (value) {
                val = 't';
            } else {
                val = 'f';
            }
            try writer.print("#{c}", .{val});
        },
        Object.character => |value| {
            switch (value) {
                '\n' => try writer.print("#\\newline", .{}),
                ' ' => try writer.print("#\\space", .{}),
                else => try writer.print("#\\{c}", .{value}),
            }
        },
    }
}

// REPL
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(allocator);

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);
        var value = try read(buffer.items, allocator);
        try write(value, stdout);
        try stdout.print("\n", .{});
        buffer.clearRetainingCapacity();
    }
}
