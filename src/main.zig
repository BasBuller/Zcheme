const std = @import("std");
const Allocator = std.mem.Allocator;

// =============================
// Objects
// =============================
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

    fn isPair(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.pair;
    }

    fn getCar(self: *Self) ?*Object {
        if (self.isPair()) {
            return self.pair.car;
        } else {
            return null;
        }
    }

    fn getCdr(self: *Self) ?*Object {
        if (self.isPair()) {
            return self.pair.cdr;
        } else {
            return null;
        }
    }
};
const Pair = struct { car: *Object, cdr: *Object };

// =============================
// State management
// =============================
const Environment = struct {
    allocator: Allocator,
    symbolLut: std.StringHashMap(Object),
    variableLut: std.AutoHashMap(*Object, *Object),
    outerEnvironment: ?*Environment,

    const Self = @This();

    fn init(allocator: Allocator, outerEnvironment: ?*Environment) Self {
        var symbolLut = std.StringHashMap(Object).init(allocator);
        var variableLut = std.AutoHashMap(*Object, *Object).init(allocator);
        return .{ .allocator = allocator, .symbolLut = symbolLut, .variableLut = variableLut, .outerEnvironment = outerEnvironment };
    }

    fn ensureTotalCapacity(self: *Self, symbolCapacity: u32, variableCapacity: u32) !void {
        try self.symbolLut.ensureTotalCapacity(symbolCapacity);
        try self.variableLut.ensureTotalCapacity(variableCapacity);
    }

    fn deinit(self: *Self) void {
        self.symbolLut.deinit();
        self.variableLut.deinit();
    }

    fn putVariable(self: *Self, symbolName: []const u8, object: *Object) !void {
        const symbol = try self.getOrPutSymbol(symbolName);
        try self.variableLut.put(symbol, object);
    }

    fn getVariableFromPointer(self: *Self, symbol: *Object) ?*Object {
        if (self.variableLut.get(symbol)) |object| {
            return object;
        } else {
            if (self.outerEnvironment) |env| {
                return env.getVariableFromPointer(symbol);
            } else {
                return null;
            }
        }
    }

    fn getVariable(self: *Self, symbolName: []const u8) ?*Object {
        if (self.symbolLut.get(symbolName)) |symbol| {
            if (self.getVariableFromPointer(symbol)) |object| {
                return object;
            } else {
                if (self.outerEnvironment) |env| {
                    return env.getVariable(symbolName);
                } else {
                    return null;
                }
            }
        } else {
            return null;
        }
    }

    fn getOrPutSymbol(self: *Self, symbolName: []const u8) !*Object {
        var lutRes = try self.symbolLut.getOrPut(symbolName);
        if (!lutRes.found_existing) {
            lutRes.value_ptr.* = Object{ .symbol = symbolName };
        }
        return lutRes.value_ptr;
    }
};

// =============================
// Parser
// =============================
const ParseError = error{
    InvalidInput,
    BufferEnd,
    UnterminatedString,
    MissingClosingParanthesis,
    InvalidCharacter,
    Overflow,
    OutOfMemory,
    InvalidSymbol,
    NotAPair,
};

const ParseResult = struct {
    object: *Object,
    remainderString: []const u8,
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

fn readCharacter(chars: []const u8, state: *Environment) ParseError!ParseResult {
    const object = try state.allocator.create(Object);

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

fn readFixnum(chars: []const u8, state: *Environment) ParseError!ParseResult {
    var idx: usize = 1;
    while ((idx < chars.len) and std.ascii.isDigit(chars[idx])) {
        idx += 1;
    }
    var num = try std.fmt.parseInt(i64, chars[0..idx], 10);

    const object = try state.allocator.create(Object);
    object.* = .{ .fixnum = num };
    return ParseResult{ .object = object, .remainderString = chars[idx..] };
}

/// Read characters between closing double quotes, while handling \n and \" espace sequences
fn readString(chars: []const u8, state: *Environment) ParseError!ParseResult {
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

    const object = try state.allocator.create(Object);
    object.* = .{ .string = chars[0..idx] };
    return ParseResult{ .object = object, .remainderString = chars[idx..] };
}

fn readSymbol(chars: []const u8, state: *Environment) ParseError!ParseResult {
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

fn readQuotedList(chars: []const u8, state: *Environment) ParseError!ParseResult {
    const quote = try state.getOrPutSymbol("quote");
    const objRes = try readObject(chars, state);
    const emptyList = try Object.createEmptyList(state.allocator);
    const quotedObject = try Object.createPair(objRes.object, emptyList, state.allocator);
    const resObject = try Object.createPair(quote, quotedObject, state.allocator);
    const res = ParseResult{ .object = resObject, .remainderString = objRes.remainderString };
    return res;
}

fn readPair(chars: []const u8, state: *Environment) ParseError!ParseResult {
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

fn readObject(chars: []const u8, state: *Environment) ParseError!ParseResult {
    var varChars = eatWhitespace(chars);
    if (varChars[0] == '#') {
        return readCharacter(varChars[1..], state);
    } else if (((varChars.len > 1) and (varChars[0] == '-') and std.ascii.isDigit(varChars[1])) or std.ascii.isDigit(varChars[0])) {
        return readFixnum(varChars, state);
    } else if (varChars[0] == '"') {
        return readString(varChars[1..], state);
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

fn read(chars: []const u8, state: *Environment) !*Object {
    var parseRes = try readObject(chars, state);
    return parseRes.object;
}

// =============================
// Evaluate
// =============================
const EvalError = error{
    InvalidExpressionType,
    UnboundVariable,
};

fn isSelfEvaluating(object: *Object) bool {
    const objType = @as(ObjectType, object.*);
    return (objType == ObjectType.boolean) or (objType == ObjectType.fixnum) or (objType == ObjectType.character) or (objType == ObjectType.string);
}

fn isVariable(expression: *Object) bool {
    return @as(ObjectType, expression.*) == ObjectType.symbol;
}

fn isTaggedList(expression: *Object, tag: *Object) bool {
    if (@as(ObjectType, expression.*) == ObjectType.pair) {
        const leadSymbol = expression.pair.car;
        const carIsSymbol = @as(ObjectType, leadSymbol.*) == ObjectType.symbol;
        return (carIsSymbol and (leadSymbol == tag));
    } else {
        return false;
    }
}

fn isQuoted(expression: *Object, state: *Environment) !bool {
    const quote = try state.getOrPutSymbol("quote");
    return isTaggedList(expression, quote);
}

fn isAssignment(expression: *Object, state: *Environment) !bool {
    const setSymbol = try state.getOrPutSymbol("set!");
    return isTaggedList(expression, setSymbol);
}

fn isDefinition(expression: *Object, state: *Environment) !bool {
    const defineSymbol = try state.getOrPutSymbol("define");
    return isTaggedList(expression, defineSymbol);
}

fn evalAssignment(expression: *Object, state: *Environment) !*Object {
    const symbol = expression.pair.cdr.pair.car.symbol;
    const value = expression.pair.cdr.pair.cdr.pair.car;
    if (state.getVariable(symbol)) |res| {
        res.* = value.*;
        return state.getVariable("ok").?;
    } else {
        return EvalError.UnboundVariable;
    }
}

fn evalDefinition(expression: *Object, state: *Environment) !*Object {
    const symbol = expression.pair.cdr.pair.car.symbol;
    const value = expression.pair.cdr.pair.cdr.pair.car;
    const lutRes = try state.variableLut.getOrPut(symbol);
    lutRes.value_ptr.* = value.*;
    return try state.getOrPutSymbol("ok");
}

fn eval(expr: *Object, state: *Environment) !*Object {
    if (isSelfEvaluating(expr)) {
        return expr;
    } else if (isVariable(expr)) {
        if (state.getVariable(expr.symbol)) |res| {
            return res;
        } else {
            return EvalError.UnboundVariable;
        }
    } else if (try isQuoted(expr, state)) {
        return expr.pair.cdr.pair.car;
    } else if (try isAssignment(expr, state)) {
        return evalAssignment(expr, state);
    } else if (try isDefinition(expr, state)) {
        return evalDefinition(expr, state);
    } else {
        return EvalError.InvalidExpressionType;
    }
}

// =============================
// Printing
// =============================
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
        Object.symbol => |value| try writer.print("{s} [{*}]", .{ value, object }),
        Object.emptyList => |_| try writer.print("()", .{}),
        Object.pair => |pair| try writePair(&pair, writer),
    }
}

// =============================
// REPL
// =============================
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var state = Environment.init(allocator, null);
    try state.ensureTotalCapacity(1024, 8192);
    _ = try state.getOrPutSymbol("quote");
    _ = try state.getOrPutSymbol("define");
    _ = try state.getOrPutSymbol("set!");
    _ = try state.getOrPutSymbol("ok");

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(allocator);

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n\n", .{});
    while (true) {
        try stdout.print("> ", .{});
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);
        if (read(buffer.items, &state)) |value| {
            try write(value, stdout);
            try stdout.print("\n", .{});
            // if (eval(value, &state)) |res| {
            //     try write(res, stdout);
            //     try stdout.print("\n", .{});
            // } else |err| {
            //     try stdout.print("Evaluation error: \"{any}\"\n", .{err});
            // }
        } else |err| {
            try stdout.print("Parsing error: \"{any}\"\n", .{err});
        }
        buffer.clearRetainingCapacity();
    }
}

test "Booleans" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = Environment.init(allocator);
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
    var state = Environment.init(allocator);
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
    var state = Environment.init(allocator);
    defer state.deinit();

    const stringTarg = Object{ .string = "abcd\\\"efg" };
    const stringRead = try read("\"abcd\\\"efg\"", &state);
    defer allocator.destroy(stringRead);
    try expect(std.mem.eql(u8, stringTarg.string, stringRead.string));
}

test "Lists" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = Environment.init(allocator);
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
    var state = Environment.init(allocator);
    defer state.deinit();

    var obj0 = try read("abc", &state);
    var obj1 = try read("abc", &state);
    try expect(obj0 == obj1);

    var obj2 = try read("abcd", &state);
    try expect(obj0 != obj2);
}
