const std = @import("std");
const Allocator = std.mem.Allocator;

// =============================
// Errors
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
const EvalError = error{
    InvalidExpressionType,
    UnboundVariable,
    UnboundConstant,
};
const LispError = error{
    InvalidInput,
    BufferEnd,
    UnterminatedString,
    MissingClosingParanthesis,
    InvalidCharacter,
    Overflow,
    OutOfMemory,
    InvalidSymbol,
    NotAPair,
    InvalidExpressionType,
    UnboundVariable,
    UnboundConstant,
};

// =============================
// Objects
// =============================
const ObjectType = enum { fixnum, boolean, character, string, emptyList, pair, symbol, primitiveProc };
const Object = union(ObjectType) {
    fixnum: i64,
    boolean: bool,
    character: u8,
    string: []const u8,
    emptyList: bool,
    pair: Pair,
    symbol: []const u8,
    primitiveProc: *const fn (arguments: *Object, state: *Environment) LispError!*Object,

    const Self = @This();

    // =============================
    // Creation utilities
    // =============================
    fn createFixnum(num: i64, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .fixnum = num };
        return object;
    }

    fn createBoolean(boolean: bool, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .boolean = boolean };
        return object;
    }

    fn createCharacter(character: u8, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .character = character };
        return object;
    }

    fn createString(string: []const u8, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .string = string };
        return object;
    }

    fn createEmptyList(allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .emptyList = true };
        return object;
    }

    fn createPair(car: *Object, cdr: *Object, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .pair = Pair{ .car = car, .cdr = cdr } };
        return object;
    }

    fn createSymbol(symbol: []const u8, allocator: Allocator) LispError!*Object {
        const object = try allocator.create(Object);
        object.* = Object{ .symbol = symbol };
        return object;
    }

    // =============================
    // Type checking
    // =============================
    fn isFixnum(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.fixnum;
    }

    fn isBoolean(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.boolean;
    }

    fn isCharacter(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.character;
    }

    fn isString(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.string;
    }

    fn isEmptyList(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.emptyList;
    }

    fn isPair(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.pair;
    }

    fn isSymbol(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.symbol;
    }

    fn isPrimitiveProc(self: *Self) bool {
        return @as(ObjectType, self.*) == ObjectType.primitiveProc;
    }
};
const Pair = struct { car: *Object, cdr: *Object };

// =============================
// Typecheck procedures
// =============================
fn returnBool(decision: bool, state: *Environment) LispError!*Object {
    if (decision) {
        return try state.getConstant("true");
    } else {
        return try state.getConstant("false");
    }
}

fn isNullProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isEmptyList();
    return try returnBool(decision, state);
}

fn isBooleanProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isBoolean();
    return try returnBool(decision, state);
}

fn isSymbolProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isSymbol();
    return try returnBool(decision, state);
}

fn isIntegerProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isFixnum();
    return try returnBool(decision, state);
}

fn isCharProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isCharacter();
    return try returnBool(decision, state);
}

fn isStringProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isString();
    return try returnBool(decision, state);
}

fn isPairProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isPair();
    return try returnBool(decision, state);
}

fn isProcedureProc(arguments: *Object, state: *Environment) LispError!*Object {
    const decision = arguments.pair.car.isPrimitiveProc();
    return try returnBool(decision, state);
}

// =============================
// Conversion procedures
// =============================
fn charToIntegerProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn integerToCharProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn numberToStringProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn stringToNumberProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn symbolToStringProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn stringToSymbolProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

// =============================
// Mathematics procedures
// =============================
fn addProc(arguments: *Object, state: *Environment) LispError!*Object {
    var res: i64 = 0;
    var args = arguments;
    while (!args.isEmptyList()) {
        res += args.pair.car.fixnum;
        args = args.pair.cdr;
    }

    const obj = try state.allocator.create(Object);
    obj.* = Object{ .fixnum = res };
    return obj;
}

fn subProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn mulProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn quotientProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn remainderProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn isNumberEqualProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn isLessThanProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn isGreaterThanProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

// =============================
// List operation procedures
// =============================
fn consProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn carProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn cdrProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn setCarProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn setCdrProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

fn listProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

// =============================
// Utility procedures
// =============================
fn isEqProc(arguments: *Object, state: *Environment) LispError!*Object {
    _ = state;
    _ = arguments;
}

// =============================
// State management
// =============================
const Environment = struct {
    allocator: Allocator,
    constantLut: std.StringHashMap(Object),
    symbolLut: std.StringHashMap(*Object),
    variableLut: std.AutoHashMap(*Object, *Object),
    outerEnvironment: ?*Environment,

    const Self = @This();

    fn init(allocator: Allocator, outerEnvironment: ?*Environment) Self {
        var constantLut = std.StringHashMap(Object).init(allocator);
        var symbolLut = std.StringHashMap(*Object).init(allocator);
        var variableLut = std.AutoHashMap(*Object, *Object).init(allocator);
        return .{
            .allocator = allocator,
            .constantLut = constantLut,
            .symbolLut = symbolLut,
            .variableLut = variableLut,
            .outerEnvironment = outerEnvironment,
        };
    }

    fn initTopLevel(allocator: Allocator) !Self {
        var state = Self.init(allocator, null);

        // Insert symbols
        try state.putSymbol("quote");
        try state.putSymbol("define");
        try state.putSymbol("set!");
        try state.putSymbol("ok");
        try state.putSymbol("if");

        // Add constant objects
        try state.putConstant("true", Object{ .boolean = true });
        try state.putConstant("false", Object{ .boolean = false });
        try state.putConstant("emptylist", Object{ .emptyList = true });

        // Add procedures
        try state.addPrimitiveProc("null?", &isNullProc);
        try state.addPrimitiveProc("boolean?", &isBooleanProc);
        try state.addPrimitiveProc("symbol?", &isSymbolProc);
        try state.addPrimitiveProc("integer?", &isIntegerProc);
        try state.addPrimitiveProc("char?", &isCharProc);
        try state.addPrimitiveProc("string?", &isStringProc);
        try state.addPrimitiveProc("pair?", &isPairProc);
        try state.addPrimitiveProc("procedure?", &isProcedureProc);

        try state.addPrimitiveProc("char->integer", &charToIntegerProc);
        try state.addPrimitiveProc("integer->char", &integerToCharProc);
        try state.addPrimitiveProc("number->string", &numberToStringProc);
        try state.addPrimitiveProc("string->number", &stringToNumberProc);
        try state.addPrimitiveProc("symbol->string", &symbolToStringProc);
        try state.addPrimitiveProc("string->symbol", &stringToSymbolProc);

        try state.addPrimitiveProc("+", &addProc);
        try state.addPrimitiveProc("-", &subProc);
        try state.addPrimitiveProc("*", &mulProc);
        try state.addPrimitiveProc("quotient", &quotientProc);
        try state.addPrimitiveProc("remainder", &remainderProc);
        try state.addPrimitiveProc("=", &isNumberEqualProc);
        try state.addPrimitiveProc("<", &isLessThanProc);
        try state.addPrimitiveProc(">", &isGreaterThanProc);

        try state.addPrimitiveProc("cons", &consProc);
        try state.addPrimitiveProc("car", &carProc);
        try state.addPrimitiveProc("cdr", &cdrProc);
        try state.addPrimitiveProc("set-car!", &setCarProc);
        try state.addPrimitiveProc("set-cdr!", &setCdrProc);
        try state.addPrimitiveProc("list", &listProc);

        try state.addPrimitiveProc("eq?", &isEqProc);

        return state;
    }

    fn ensureTotalCapacity(self: *Self, symbolCapacity: u32, variableCapacity: u32) !void {
        try self.symbolLut.ensureTotalCapacity(symbolCapacity);
        try self.variableLut.ensureTotalCapacity(variableCapacity);
    }

    fn deinit(self: *Self) void {
        // Variables
        var valueIterator = self.variableLut.valueIterator();
        while (valueIterator.next()) |item| {
            self.allocator.destroy(item.*);
        }
        self.variableLut.deinit();

        // Symbols
        var symbolIterator = self.symbolLut.iterator();
        while (symbolIterator.next()) |item| {
            self.allocator.free(item.key_ptr.*);
            self.allocator.destroy(item.value_ptr.*);
        }
        self.symbolLut.deinit();

        var constantIterator = self.constantLut.keyIterator();
        while (constantIterator.next()) |key| {
            self.allocator.free(key.*);
        }
        self.constantLut.deinit();
    }

    fn putConstant(self: *Self, constantName: []const u8, object: Object) !void {
        const constantKey = try self.allocator.alloc(u8, constantName.len);
        @memcpy(constantKey, constantName);
        try self.constantLut.put(constantKey, object);
    }

    fn getConstant(self: *Self, constantName: []const u8) !*Object {
        if (self.outerEnvironment) |outerEnv| {
            return try outerEnv.getConstant(constantName);
        } else {
            if (self.constantLut.getPtr(constantName)) |objPtr| {
                return objPtr;
            } else {
                return EvalError.UnboundConstant;
            }
        }
    }

    fn putVariable(self: *Self, symbolName: []const u8, object: *Object) !void {
        const symbol = try self.getInsertSymbol(symbolName);
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

    /// Puts the key string on the heap, then creates symbol lut entry based on pointed to the key string on the heap.
    /// Reason for first storing key on the heap is that otherwise there is a mutating reference to the array used for storing IO.
    fn putSymbol(self: *Self, symbolName: []const u8) !void {
        const symbolKey = try self.allocator.alloc(u8, symbolName.len);
        @memcpy(symbolKey, symbolName);
        var symbol = try self.allocator.create(Object);
        symbol.* = Object{ .symbol = symbolKey };
        try self.symbolLut.put(symbolKey, symbol);
    }

    fn getSymbol(self: *Self, symbolName: []const u8) ?*Object {
        return self.symbolLut.get(symbolName);
    }

    fn getInsertSymbol(self: *Self, symbolName: []const u8) !*Object {
        if (!self.symbolLut.contains(symbolName)) {
            try self.putSymbol(symbolName);
        }
        return self.getSymbol(symbolName).?;
    }

    fn addPrimitiveProc(self: *Self, procName: []const u8, proc: *const fn (arguments: *Object, state: *Environment) LispError!*Object) !void {
        const addObject = try self.allocator.create(Object);
        addObject.* = Object{ .primitiveProc = proc };
        try self.putVariable(procName, addObject);
    }

    // Utils
    fn printSymbols(self: *Self, writer: std.fs.File.Writer) !void {
        var iter = self.symbolLut.iterator();
        try writer.print("\n", .{});
        while (iter.next()) |entry| {
            try writer.print("key: {s}[{*}] --- value: {s}[{*}]\n", .{ entry.key_ptr.*, entry.key_ptr, entry.value_ptr.*.symbol, entry.value_ptr });
        }
    }
};

// =============================
// Parser
// =============================
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

fn readCharacter(chars: []const u8, state: *Environment) LispError!ParseResult {
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

fn readFixnum(chars: []const u8, state: *Environment) LispError!ParseResult {
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
fn readString(chars: []const u8, state: *Environment) LispError!ParseResult {
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

fn readSymbol(chars: []const u8, state: *Environment) LispError!ParseResult {
    var idx: usize = 1;
    while ((idx < chars.len) and
        (isInitial(chars[idx]) or std.ascii.isDigit(chars[idx]) or (chars[idx] == '+') or (chars[idx] == '-')))
    {
        idx += 1;
    }

    if ((idx == chars.len) or
        ((idx < chars.len) and isDelimiter(chars[idx])))
    {
        const symbol = try state.getInsertSymbol(chars[0..idx]);
        return ParseResult{ .object = symbol, .remainderString = chars[idx..] };
    } else {
        return ParseError.InvalidSymbol;
    }
}

fn readQuotedList(chars: []const u8, state: *Environment) LispError!ParseResult {
    const quote = try state.getInsertSymbol("quote");
    const objRes = try readObject(chars, state);
    const emptyList = try state.getConstant("emptyList");
    const quotedObject = try Object.createPair(objRes.object, emptyList, state.allocator);
    const resObject = try Object.createPair(quote, quotedObject, state.allocator);
    const res = ParseResult{ .object = resObject, .remainderString = objRes.remainderString };
    return res;
}

fn readPair(chars: []const u8, state: *Environment) LispError!ParseResult {
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

fn readObject(chars: []const u8, state: *Environment) LispError!ParseResult {
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
    const parseRes = try readObject(chars, state);
    return parseRes.object;
}

// =============================
// Evaluate
// =============================

fn isSelfEvaluating(object: *Object) bool {
    return object.isBoolean() or object.isFixnum() or object.isCharacter() or object.isString();
}

fn isVariable(expression: *Object) bool {
    return expression.isSymbol();
}

fn isTaggedList(expression: *Object, tag: *Object) bool {
    if (expression.isPair()) {
        const leadSymbol = expression.pair.car;
        return (leadSymbol.isSymbol() and (leadSymbol == tag));
    } else {
        return false;
    }
}

fn isQuoted(expression: *Object, state: *Environment) bool {
    const quote = state.getSymbol("quote").?;
    return isTaggedList(expression, quote);
}

fn isAssignment(expression: *Object, state: *Environment) bool {
    const setSymbol = state.getSymbol("set!").?;
    return isTaggedList(expression, setSymbol);
}

fn isDefinition(expression: *Object, state: *Environment) bool {
    const defineSymbol = state.getSymbol("define").?;
    return isTaggedList(expression, defineSymbol);
}

fn isIf(expression: *Object, state: *Environment) bool {
    const ifSymbol = state.getSymbol("if").?;
    return isTaggedList(expression, ifSymbol);
}

fn isTrue(expression: *Object) bool {
    if (expression.isBoolean()) {
        return expression.boolean;
    } else {
        return true;
    }
}

fn isApplication(expression: *Object) bool {
    return expression.isPair();
}

fn evalIf(expression: *Object, state: *Environment) !*Object {
    const predicate = expression.pair.cdr.pair.car;
    if (isTrue(predicate)) {
        return expression.pair.cdr.pair.cdr.pair.car;
    } else {
        const resObj = expression.pair.cdr.pair.cdr.pair.cdr;
        if (resObj.isEmptyList()) {
            const obj = try state.allocator.create(Object);
            obj.* = Object{ .boolean = false };
            return obj;
        } else {
            return resObj.pair.car;
        }
    }
}

fn evalAssignment(expression: *Object, state: *Environment) !*Object {
    const symbol = expression.pair.cdr.pair.car.symbol;
    const value = expression.pair.cdr.pair.cdr.pair.car;
    if (state.getVariable(symbol)) |res| {
        res.* = value.*;
        return state.getSymbol("ok").?;
    } else {
        return EvalError.UnboundVariable;
    }
}

fn evalDefinition(expression: *Object, state: *Environment) !*Object {
    const symbolName = expression.pair.cdr.pair.car.symbol;
    const value = expression.pair.cdr.pair.cdr.pair.car;
    try state.putVariable(symbolName, value);
    return state.getSymbol("ok").?;
}

fn listOfValues(expressions: *Object, state: *Environment) LispError!*Object {
    if (expressions.isEmptyList()) {
        return Object.createEmptyList(state.allocator);
    } else {
        const car = try eval(expressions.pair.car, state);
        const cdr = try listOfValues(expressions.pair.cdr, state);
        return try Object.createPair(car, cdr, state.allocator);
    }
}

fn evalApplication(expression: *Object, state: *Environment) LispError!*Object {
    const procedure = try eval(expression.pair.car, state);
    const arguments = try listOfValues(expression.pair.cdr, state);
    return procedure.primitiveProc(arguments, state);
}

fn eval(expression: *Object, state: *Environment) LispError!*Object {
    var expr = expression;
    while (true) {
        if (isSelfEvaluating(expr)) {
            return expr;
        } else if (isVariable(expr)) {
            if (state.getVariable(expr.symbol)) |res| {
                return res;
            } else {
                return EvalError.UnboundVariable;
            }
        } else if (isQuoted(expr, state)) {
            return expr.pair.cdr.pair.car;
        } else if (isAssignment(expr, state)) {
            return evalAssignment(expr, state);
        } else if (isDefinition(expr, state)) {
            return evalDefinition(expr, state);
        } else if (isIf(expr, state)) {
            expr = try evalIf(expr, state);
        } else if (isApplication(expr)) {
            return evalApplication(expr, state);
        } else {
            return EvalError.InvalidExpressionType;
        }
    }
}

// =============================
// Printing
// =============================
fn recurseList(pair: *const Pair, writer: std.fs.File.Writer) std.fs.File.Writer.Error!void {
    try write(pair.car, writer);
    if (pair.cdr.isPair()) {
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
        Object.primitiveProc => |_| try writer.print("#<procedure>", .{}),
    }
}

// =============================
// REPL
// =============================
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var state = try Environment.initTopLevel(allocator);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(allocator);

    try stdout.print("\nWelcome to ZLisp (which is a Scheme)\n\n", .{});
    while (true) {
        // try state.printSymbols(stdout);
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

// =============================
// Testing
// =============================
test "Booleans" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = try Environment.initTopLevel(allocator);
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
    var state = try Environment.initTopLevel(allocator);
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
    var state = try Environment.initTopLevel(allocator);
    defer state.deinit();

    const stringTarg = Object{ .string = "abcd\\\"efg" };
    const stringRead = try read("\"abcd\\\"efg\"", &state);
    defer allocator.destroy(stringRead);
    try expect(std.mem.eql(u8, stringTarg.string, stringRead.string));
}

test "Lists" {
    const expect = std.testing.expect;
    var allocator = std.testing.allocator;
    var state = try Environment.initTopLevel(allocator);
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
    var state = try Environment.initTopLevel(allocator);
    defer state.deinit();

    const obj0 = try read("abc", &state);
    const obj1 = try read("abc", &state);
    try expect(obj0 == obj1);

    const obj2 = try read("abcd", &state);
    try expect(obj0 != obj2);
}
