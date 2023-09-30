const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const Object = union(enum) {
    fixnum: i64,
    boolean: bool,
    character: u8,
    string: []const u8,
    emptyList: bool,
    pair: struct { car: *Object, cdr: *Object },
};

// Parser
const ParseError = error{
    InvalidInput,
    InvalidCharacter,
    BufferEnd,
    UnterminatedString,
    MissingClosingParanthesis,
};

fn isDelimiter(char: u8) bool {
    return (std.ascii.isWhitespace(char) or (char == '(') or (char == ')') or (char == '"') or (char == ';'));
}

fn eatWhitespace(slice: []const u8) []const u8 {
    var idx: usize = 0;
    while ((idx < slice.len) and (std.ascii.isWhitespace(slice[idx]))) {
        idx += 1;
    }
    return slice[idx..];
}

fn readCharacter(chars: []const u8, object: *Object) ![]const u8 {
    switch (chars[0]) {
        't' => {
            object.* = .{ .boolean = true };
            return chars[1..];
        },
        'f' => {
            object.* = .{ .boolean = false };
            return chars[1..];
        },
        '\\' => {
            if (chars.len < 6) {
                return ParseError.BufferEnd;
            } else if (std.mem.eql(u8, chars[1..6], "space")) {
                object.* = .{ .character = ' ' };
                return chars[5..];
            } else if (std.mem.eql(u8, chars[1..8], "newline")) {
                object.* = .{ .character = '\n' };
                return chars[8..];
            } else {
                return ParseError.InvalidCharacter;
            }
        },
        else => return ParseError.InvalidInput,
    }
}

fn readFixnum(chars: []const u8, object: *Object) ![]const u8 {
    var idx: usize = 1;
    while ((idx < chars.len) and std.ascii.isDigit(chars[idx])) {
        idx += 1;
    }
    var num = try std.fmt.parseInt(i64, chars[0..idx], 10);
    object.* = .{ .fixnum = num };
    return chars[idx..];
}

/// Read characters between closing double quotes, while handling \n and \" espace sequences
fn readString(chars: []const u8, object: *Object) ![]const u8 {
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

    object.* = .{ .string = chars[0..idx] };
    return chars[idx..];
}

fn readObject(chars: []const u8, object: *Object, allocator: Allocator) ![]const u8 {
    var varChars = eatWhitespace(chars);
    if (varChars[0] == '#') {
        return readCharacter(varChars[1..], object);
    } else if ((varChars[0] == '-') or std.ascii.isDigit(varChars[0])) {
        return readFixnum(varChars, object);
    } else if (varChars[0] == '"') {
        return readString(varChars[1..], object);
    } else if (varChars[0] == '(') {
        return readPair(varChars[1..], object, allocator);
    } else {
        return ParseError.InvalidInput;
    }
}

fn readPair(chars: []const u8, object: *Object, allocator: Allocator) ![]const u8 {
    if (chars[0] == ')') {
        object.* = .{ .emptyList = true };
        return chars[1..];
    }

    // First object of pair
    var car = try allocator.create(Object);
    var varChars = try readObject(chars, object, allocator);
    varChars = eatWhitespace(varChars);

    // Second object of pair
    var cdr = try allocator.create(Object);
    varChars = try readObject(varChars, object, allocator);
    varChars = eatWhitespace(varChars);

    // Closing bracket and create final object
    if (varChars[0] != ')') {
        object.* = .{ .pair = .{ .car = car, .cdr = cdr } };
        return varChars[1..];
    } else {
        return ParseError.MissingClosingParanthesis;
    }
}

fn read(chars: []const u8, allocator: Allocator) !*Object {
    var object = try allocator.create(Object);
    _ = try readObject(chars, object, allocator);
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
        Object.pair => |_| try writer.print("WIP\n", .{}),
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
            ParseError.InvalidCharacter => try stdout.print("Invalid character, please try again\n", .{}),
            ParseError.BufferEnd => try stdout.print("Seems like an incomplete command, please try again\n", .{}),
            ParseError.UnterminatedString => try stdout.print("Unterminated string, seems you forgot closing quotesn", .{}),
            else => try stdout.print("Unclear error", .{}),
        }
        buffer.clearRetainingCapacity();
    }
}

test "Reading of single objects" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;

    // Booleans
    const trueTarg = Object{ .boolean = true };
    const trueRead = try read("#t", allocator);
    defer allocator.destroy(trueRead);
    try expect(trueTarg.boolean == trueRead.boolean);

    const falseTarg = Object{ .boolean = false };
    const falseRead = try read("#f", allocator);
    defer allocator.destroy(falseRead);
    try expect(falseTarg.boolean == falseRead.boolean);

    // Fixnums
    const posNumTarg = Object{ .fixnum = 5 };
    const posNumRead = try read("5", allocator);
    defer allocator.destroy(posNumRead);
    try expect(posNumTarg.fixnum == posNumRead.fixnum);

    const negNumTarg = Object{ .fixnum = -5 };
    const negNumRead = try read("-5", allocator);
    defer allocator.destroy(negNumRead);
    try expect(negNumTarg.fixnum == negNumRead.fixnum);

    // String
    const stringTarg = Object{ .string = "abcd\\\"efg" };
    const stringRead = try read("\"abcd\\\"efg\"", allocator);
    defer allocator.destroy(stringRead);
    try expect(std.mem.eql(u8, stringTarg.string, stringRead.string));

    // (Empty, for now) list
    const emptyListTarg = Object{ .emptyList = true };
    const emptyListRead = try read("()", allocator);
    defer allocator.destroy(emptyListRead);
    try expect(emptyListTarg.emptyList == emptyListRead.emptyList);
}
