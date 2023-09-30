const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const Object = union(enum) {
    fixnum: i64,
    boolean: bool,
    character: u8,
    string: []u8,
    emptyList: bool,
};

// Parser
const ParseError = error{
    InvalidInput,
    BufferEnd,
    UnterminatedString,
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

fn matchExpectedString(slice: []const u8, target: []const u8, output: u8) ParseError!u8 {
    if (std.mem.eql(u8, slice, target)) {
        return output;
    } else {
        return ParseError.InvalidInput;
    }
}

fn startS(chars: []u8) !u8 {
    if (chars.len > 0) {
        switch (chars[0]) {
            'p' => return matchExpectedString(chars[1..], "ace", ' '),
            else => return ParseError.InvalidInput,
        }
    } else {
        return ParseError.BufferEnd;
    }
}

fn startN(chars: []u8) !u8 {
    if (chars.len > 0) {
        switch (chars[0]) {
            'e' => return matchExpectedString(chars[1..], "wline", '\n'),
            else => return ParseError.InvalidInput,
        }
    } else {
        return ParseError.BufferEnd;
    }
}

fn readCharacter(chars: []u8) !u8 {
    if (chars.len > 0) {
        switch (chars[0]) {
            's' => return startS(chars[1..]),
            'n' => return startN(chars[1..]),
            else => return ParseError.InvalidInput,
        }
    } else {
        return ParseError.BufferEnd;
    }
}

fn readFixnum(chars: []u8) !i64 {
    var idx: usize = 1;
    while ((idx < chars.len) and std.ascii.isDigit(chars[idx])) {
        idx += 1;
    }
    var num = try std.fmt.parseInt(i64, chars[0..idx], 10);
    return num;
}

/// Read characters between closing double quotes, while handling \n and \" espace sequences
fn readString(chars: []u8) ![]u8 {
    var escapeOn: bool = false;
    var idx: usize = 0;
    while ((chars[idx] != '"') or escapeOn) {
        if (chars[idx] == '\\') {
            escapeOn = true;
        } else {
            escapeOn = false;
        }
        idx += 1;
        if (idx == chars.len) return ParseError.UnterminatedString;
    }
    return chars[0..idx];
}

fn read(chars: []u8, allocator: Allocator) !*Object {
    var object = try allocator.create(Object);

    var varChars = eatWhitespace(chars);
    while (varChars[0] != '\n') {
        if (varChars[0] == '#') { // Boolean or character
            switch (varChars[1]) {
                't' => object.* = .{ .boolean = true },
                'f' => object.* = .{ .boolean = false },
                '\\' => {
                    var charVal = try readCharacter(varChars[2..]);
                    object.* = .{ .character = charVal };
                },
                else => return ParseError.InvalidInput,
            }
        } else if ((varChars[0] == '-') or std.ascii.isDigit(varChars[0])) { // Fixnum
            object.* = .{ .fixnum = try readFixnum(varChars) };
        } else if (varChars[0] == '"') { // String
            object.* = .{ .string = try readString(varChars[1..]) };
        } else if (varChars[0] == '(') {
            switch (varChars[1]) {
                ')' => object.* = .{ .emptyList = true },
                else => return ParseError.InvalidInput,
            }
        } else {
            return ParseError.InvalidInput;
        }
        varChars = eatWhitespace(varChars);
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
        Object.string => |value| try writer.print("\"{s}\"\n", .{value}),
        Object.emptyList => |_| try writer.print("()\n", .{}),
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
        if (read(buffer.items, allocator)) |value| {
            try write(value, stdout);
            try stdout.print("\n", .{});
        } else |err| switch (err) {
            ParseError.InvalidInput => try stdout.print("Invalid input, please try again\n", .{}),
            ParseError.BufferEnd => try stdout.print("Seems like an incomplete command, please try again\n", .{}),
            ParseError.UnterminatedString => try stdout.print("Unterminated string, seems you forgot closing quotes.\n", .{}),
            else => try stdout.print("Unclear error", .{}),
        }
        buffer.clearRetainingCapacity();
    }
}
