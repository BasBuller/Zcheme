const std = @import("std");
const Allocator = std.mem.Allocator;

// Objects
const ObjectType = enum { fixnum, boolean, character, string, emptyList, pair, symbol };
const Object = union(ObjectType) {
    fixnum: i64,
    boolean: bool,
    character: u8,
    string: []const u8,
    emptyList: bool,
    pair: Pair,
    symbol: []const u8,

    const Self = @This();

    fn createEmptyList(allocator: Allocator) ParseError!*Object {
        const empty = try allocator.create(Object);
        empty.* = Object{ .emptyList = true };
        return empty;
    }

    fn createPair(car: *Object, cdr: *Object, allocator: Allocator) ParseError!*Object {
        const pair = try allocator.create(Object);
        pair.* = Object{ .pair = Pair{ .car = car, .cdr = cdr } };
        return pair;
    }
};
const Pair = struct { car: *Object, cdr: *Object };

// State management
const SymbolLut = std.StringHashMap(Object);
const State = struct {
    allocator: Allocator,
    symbolLut: SymbolLut,

    const Self = @This();

    fn init(allocator: Allocator) Self {
        var lut = SymbolLut.init(allocator);
        return .{ .allocator = allocator, .symbolLut = lut };
    }

    fn deinit(self: *Self) void {
        self.symbolLut.deinit();
    }

    fn getOrPutSymbol(self: *Self, symbolName: []const u8) ParseError!*Object {
        var lutRes = try self.symbolLut.getOrPut(symbolName);
        if (!lutRes.found_existing) {
            lutRes.value_ptr.* = Object{ .symbol = symbolName };
        }
        return lutRes.value_ptr;
    }
};

const ParseResult = struct {
    object: *Object,
    remainderString: []const u8,
};

// Parser
const ParseError = error{
    InvalidInput,
    BufferEnd,
    UnterminatedString,
    MissingClosingParanthesis,
    InvalidCharacter,
    Overflow,
    OutOfMemory,
    InvalidSymbol,
};

fn isDelimiter(char: u8) bool {
    return (std.ascii.isWhitespace(char) or (char == '(') or (char == ')') or (char == '"') or (char == ';'));
}

fn isInitial(char: u8) bool {
    return (std.ascii.isAlphabetic(char) or (char == '*') or (char == '/') or (char == '<') or (char == '>') or (char == '=') or (char == '?') or (char == '!'));
}

fn eatWhitespace(slice: []const u8) []const u8 {
    var idx: usize = 0;
    while ((idx < slice.len) and (std.ascii.isWhitespace(slice[idx]))) {
        idx += 1;
    }
    return slice[idx..];
}

fn readCharacter(chars: []const u8, allocator: Allocator) ParseError!ParseResult {
    var object = try allocator.create(Object);
    var resString: []const u8 = undefined;
    switch (chars[0]) {
        't' => {
            object.* = .{ .boolean = true };
            resString = chars[1..];
        },
        'f' => {
            object.* = .{ .boolean = false };
            resString = chars[1..];
        },
        '\\' => {
            if (chars.len < 6) {
                return ParseError.BufferEnd;
            } else if (std.mem.eql(u8, chars[1..6], "space")) {
                object.* = .{ .character = ' ' };
                resString = chars[5..];
            } else if (std.mem.eql(u8, chars[1..8], "newline")) {
                object.* = .{ .character = '\n' };
                resString = chars[8..];
            } else {
                return ParseError.InvalidCharacter;
            }
        },
        else => return ParseError.InvalidInput,
    }
    return ParseResult{ .object = object, .remainderString = resString };
}

fn readFixnum(chars: []const u8, allocator: Allocator) ParseError!ParseResult {
    var idx: usize = 1;
    while ((idx < chars.len) and std.ascii.isDigit(chars[idx])) {
        idx += 1;
    }
    var num = try std.fmt.parseInt(i64, chars[0..idx], 10);

    var object = try allocator.create(Object);
    object.* = .{ .fixnum = num };
    return ParseResult{ .object = object, .remainderString = chars[idx..] };
}

/// Read characters between closing double quotes, while handling \n and \" espace sequences
fn readString(chars: []const u8, allocator: Allocator) ParseError!ParseResult {
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

    var object = try allocator.create(Object);
    object.* = .{ .string = chars[0..idx] };
    return ParseResult{ .object = object, .remainderString = chars[idx..] };
}

fn readSymbol(chars: []const u8, state: *State) ParseError!ParseResult {
    var idx: usize = 1;
    while ((idx < chars.len) and
        (isInitial(chars[idx]) or std.ascii.isDigit(chars[idx]) or (chars[idx] == '+') or (chars[idx] == '-')))
    {
        idx += 1;
    }

    if ((idx == chars.len) or
        ((idx < chars.len) and isDelimiter(chars[idx])))
    {
        const object = try state.getOrPutSymbol(chars[0..idx]);
        return ParseResult{ .object = object, .remainderString = chars[idx..] };
    } else {
        return ParseError.InvalidSymbol;
    }
}

fn readQuotedList(chars: []const u8, state: *State) ParseError!ParseResult {
    const quote = try state.getOrPutSymbol("quote");
    const objRes = try readObject(chars, state);
    const emptyList = try Object.createEmptyList(state.allocator);
    const quotedObject = try Object.createPair(objRes.object, emptyList, state.allocator);
    const resObject = try Object.createPair(quote, quotedObject, state.allocator);
    const res = ParseResult{ .object = resObject, .remainderString = objRes.remainderString };
    return res;
}

fn readPair(chars: []const u8, state: *State) ParseError!ParseResult {
    if (chars[0] == ')') {
        const object = try Object.createEmptyList(state.allocator);
        return ParseResult{ .object = object, .remainderString = chars[1..] };
    }

    // First object of pair
    var parseRes = try readObject(chars, state);
    var car = parseRes.object;

    // Deal with whitespace and optional dot
    var varChars = eatWhitespace(parseRes.remainderString);
    if (varChars.len == 0) {
        return ParseError.MissingClosingParanthesis;
    }

    var cdr: *Object = undefined;
    if (varChars[0] == '.') varChars = eatWhitespace(varChars[1..]);
    if (varChars.len == 0) {
        return ParseError.MissingClosingParanthesis;
    } else if (varChars[0] == ')') {
        cdr = try Object.createEmptyList(state.allocator);
        varChars = varChars[1..];
    } else {
        const cdrRes = try readPair(varChars, state);
        cdr = cdrRes.object;
        varChars = cdrRes.remainderString;
    }

    const object = try Object.createPair(car, cdr, state.allocator);
    return ParseResult{ .object = object, .remainderString = varChars };
}

fn readObject(chars: []const u8, state: *State) ParseError!ParseResult {
    var varChars = eatWhitespace(chars);
    if (varChars[0] == '#') {
        return readCharacter(varChars[1..], state.allocator);
    } else if (((varChars.len > 1) and (varChars[0] == '-') and std.ascii.isDigit(varChars[1])) or std.ascii.isDigit(varChars[0])) {
        return readFixnum(varChars, state.allocator);
    } else if (varChars[0] == '"') {
        return readString(varChars[1..], state.allocator);
    } else if (isInitial(varChars[0]) or ((varChars.len > 1) and ((varChars[0] == '+') or (varChars[0] == '-')) and isDelimiter(varChars[1]))) {
        return readSymbol(varChars, state);
    } else if (varChars[0] == '(') {
        return readPair(varChars[1..], state);
    } else if (varChars[0] == '\'') {
        return readQuotedList(varChars[1..], state);
    } else {
        return ParseError.InvalidInput;
    }
}

fn read(chars: []const u8, state: *State) !*Object {
    var parseRes = try readObject(chars, state);
    return parseRes.object;
}

// Evaluate
const EvalError = error{
    InvalidExpressionType,
};

fn isSelfEvaluating(object: *Object) bool {
    const objType = @as(ObjectType, object.*);
    return (objType == ObjectType.boolean) or (objType == ObjectType.fixnum) or (objType == ObjectType.character) or (objType == ObjectType.string);
}

fn isTaggedList(expression: *Object, tag: *Object) bool {
    if (@as(ObjectType, expression.*) == ObjectType.pair) {
        const carIsSymbol = @as(ObjectType, expression.pair.car.*) == ObjectType.symbol;
        return (carIsSymbol and (expression.pair.car == tag));
    } else {
        return false;
    }
}

fn isQuoted(expression: *Object, state: *State) !bool {
    const quote = try state.getOrPutSymbol("quote");
    return isTaggedList(expression, quote);
}

fn eval(expr: *Object, state: *State) !*Object {
    if (isSelfEvaluating(expr)) {
        return expr;
    } else if (try isQuoted(expr, state)) {
        return expr.pair.cdr.pair.car;
    } else {
        return EvalError.InvalidExpressionType;
    }
}

// Printing
fn recurseList(pair: *const Pair, writer: std.fs.File.Writer) std.fs.File.Writer.Error!void {
    try write(pair.car, writer);
    if (@as(ObjectType, pair.cdr.*) == ObjectType.pair) {
        try writer.print(" ", .{});
        try recurseList(&pair.cdr.pair, writer);
    }
}

fn writePair(pair: *const Pair, writer: std.fs.File.Writer) std.fs.File.Writer.Error!void {
    try writer.print("(", .{});
    try recurseList(pair, writer);
    try writer.print(")", .{});
}

fn write(object: *Object, writer: std.fs.File.Writer) std.fs.File.Writer.Error!void {
    switch (object.*) {
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
        Object.string => |value| try writer.print("\"{s}\"", .{value}),
        Object.symbol => |value| try writer.print("{s}", .{value}),
        Object.emptyList => |_| try writer.print("()", .{}),
        Object.pair => |pair| try writePair(&pair, writer),
    }
}

// REPL
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var state = State.init(allocator);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(allocator);

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);
        if (read(buffer.items, &state)) |value| {
            if (eval(value, &state)) |res| {
                try write(res, stdout);
                try stdout.print("\n", .{});
            } else |err| {
                try stdout.print("Evaluation error: \"{any}\"\n", .{err});
            }
        } else |err| {
            try stdout.print("Parsing error: \"{any}\"\n", .{err});
        }
        buffer.clearRetainingCapacity();
    }
}

test "Booleans" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = State.init(allocator);
    defer state.deinit();

    const trueTarg = Object{ .boolean = true };
    const trueRead = try read("#t", &state);
    defer allocator.destroy(trueRead);
    try expect(trueTarg.boolean == trueRead.boolean);

    const falseTarg = Object{ .boolean = false };
    const falseRead = try read("#f", &state);
    defer allocator.destroy(falseRead);
    try expect(falseTarg.boolean == falseRead.boolean);
}

test "Fixnums" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = State.init(allocator);
    defer state.deinit();

    const posNumTarg = Object{ .fixnum = 5 };
    const posNumRead = try read("5", &state);
    defer allocator.destroy(posNumRead);
    try expect(posNumTarg.fixnum == posNumRead.fixnum);

    const negNumTarg = Object{ .fixnum = -5 };
    const negNumRead = try read("-5", &state);
    defer allocator.destroy(negNumRead);
    try expect(negNumTarg.fixnum == negNumRead.fixnum);
}

test "Strings" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = State.init(allocator);
    defer state.deinit();

    const stringTarg = Object{ .string = "abcd\\\"efg" };
    const stringRead = try read("\"abcd\\\"efg\"", &state);
    defer allocator.destroy(stringRead);
    try expect(std.mem.eql(u8, stringTarg.string, stringRead.string));
}

test "Lists" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = State.init(allocator);
    defer state.deinit();

    // (Empty, for now) list
    const emptyListTarg = Object{ .emptyList = true };
    const emptyListRead = try read("()", &state);
    defer allocator.destroy(emptyListRead);
    try expect(emptyListTarg.emptyList == emptyListRead.emptyList);

    // Simple pair
    var car = Object{ .fixnum = 1 };
    var cdar = Object{ .fixnum = 2 };
    var cddr = Object{ .emptyList = true };
    var cdr = Object{ .pair = Pair{ .car = &cdar, .cdr = &cddr } };
    const basicPairTarg = Object{ .pair = Pair{ .car = &car, .cdr = &cdr } };
    const basicPairRead = try read("(1 2)", &state);
    defer allocator.destroy(basicPairRead);
    defer allocator.destroy(basicPairRead.pair.car);
    defer allocator.destroy(basicPairRead.pair.cdr);
    defer allocator.destroy(basicPairRead.pair.cdr.pair.car);
    defer allocator.destroy(basicPairRead.pair.cdr.pair.cdr);
    try expect(basicPairTarg.pair.car.fixnum == basicPairRead.pair.car.fixnum);
    try expect(basicPairTarg.pair.cdr.pair.car.fixnum == basicPairRead.pair.cdr.pair.car.fixnum);
    try expect(basicPairTarg.pair.cdr.pair.cdr.emptyList == basicPairRead.pair.cdr.pair.cdr.emptyList);

    // Pair with empty list as second item
    car = Object{ .fixnum = 1 };
    cdr = Object{ .emptyList = true };
    const emptyPairTarg = Object{ .pair = Pair{ .car = &car, .cdr = &cdr } };
    const emptyPairRead = try read("(1)", &state);
    defer allocator.destroy(emptyPairRead);
    defer allocator.destroy(emptyPairRead.pair.car);
    defer allocator.destroy(emptyPairRead.pair.cdr);
    try expect(emptyPairTarg.pair.car.fixnum == emptyPairRead.pair.car.fixnum);
    try expect(emptyPairTarg.pair.cdr.emptyList == emptyPairRead.pair.cdr.emptyList);
}

test "Symbols" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = State.init(allocator);
    defer state.deinit();

    var obj0 = try read("abc", &state);
    var obj1 = try read("abc", &state);
    try expect(obj0 == obj1);

    var obj2 = try read("abcd", &state);
    try expect(obj0 != obj2);
}
