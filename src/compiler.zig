const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;
const stderr = std.io.getStdErr().writer();

const vm = @import("vm.zig");
const InterpretError = vm.InterpretError;

const Chunk = @import("chunk.zig");
const Op = Chunk.OpCode;

const scanner = @import("scanner.zig");
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const Value = @import("value.zig").Value;

const object = @import("object.zig");
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;

const debug = @import("debug.zig");

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const Precedence = enum(u8) {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
};

const ParseFn: type = fn (canAssign: bool) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
        return ParseRule{
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }
};

const Local = struct {
    name: Token,
    depth: i32,
    isCaptured: bool,
};

const Upvalue = struct {
    index: u8,
    isLocal: bool,

    pub fn init(index: u8, isLocal: bool) Upvalue {
        return Upvalue{
            .index = index,
            .isLocal = isLocal,
        };
    }
};

const FunctionType = enum {
    Function,
    Script,
};

var current: ?*Compiler = null;

/// Function Compiler State
const Compiler = struct {
    enclosing: ?*Compiler = null,

    func: *ObjFunction,
    funcType: FunctionType,

    locals: [math.maxInt(u8) + 1]Local = undefined,
    localCount: i32 = 0,

    upvalues: std.ArrayList(Upvalue),

    scopeDepth: i32 = 0,

    allocator: *Allocator,

    pub fn init(allocator: *Allocator, funcType: FunctionType) !Compiler {
        var compiler = Compiler{
            .func = try ObjFunction.create(allocator),
            .funcType = funcType,
            .allocator = allocator,
            .localCount = 1,
            .upvalues = std.ArrayList(Upvalue).init(allocator),
            .enclosing = current,
        };

        if (funcType != FunctionType.Script)
            compiler.func.name = try ObjString.copy(allocator, parser.previous.chars);

        compiler.locals[0] = Local{
            .name = Token{ .chars = "", .typ = TokenType.AND, .line = 0 },
            .depth = 0,
            .isCaptured = false,
        };

        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.upvalues.deinit();
    }
};

var parser: Parser = undefined;

fn currentChunk() *Chunk {
    return &current.?.func.chunk;
}

fn codeAt(offset: i32) u8 {
    return @as(u8, @enumToInt(currentChunk().code.items[offset]));
}

fn codeSet(offset: i32, byte: u8) void {
    currentChunk().code.items[@intCast(usize, offset)] = byte;
}

fn codeAdd(byte: u8) !void {
    try currentChunk().write(byte, @intCast(u32, parser.previous.line));
}

fn codeLen() i32 {
    return @intCast(i32, currentChunk().code.items.len);
}

fn errorAt(token: *Token, msg: []const u8) void {
    if (parser.panicMode) return;
    parser.panicMode = true;

    stderr.print("[line {}] Error", .{token.line}) catch |_| {};

    if (token.typ == TokenType.EOF) {
        stderr.print(" at end", .{}) catch |_| {};
    } else if (token.typ == TokenType.ERROR) {} else {
        stderr.print(" at '{}'", .{token.chars}) catch |_| {};
    }

    stderr.print(": {}\n", .{msg}) catch |_| {};
    parser.hadError = true;
}

fn errored(msg: []const u8) void {
    errorAt(&parser.previous, msg);
}

fn errorCurrent(msg: []const u8) void {
    errorAt(&parser.current, msg);
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.scanToken();
        if (parser.current.typ != TokenType.ERROR) break;

        errorCurrent(parser.current.chars);
    }
}

fn consume(typ: TokenType, msg: []const u8) void {
    if (parser.current.typ == typ) {
        advance();
        return;
    }
    errorCurrent(msg);
}

fn check(typ: TokenType) bool {
    return parser.current.typ == typ;
}

fn match(typ: TokenType) bool {
    if (!check(typ)) return false;
    advance();
    return true;
}

fn emitOp(op: Op) !void {
    try codeAdd(@enumToInt(op));
}

fn emitOps(o1: Op, o2: Op) !void {
    try emitOp(o1);
    try emitOp(o2);
}

fn emitByte(byte: u8) !void {
    try codeAdd(byte);
}

fn emitBytes(b1: u8, b2: u8) !void {
    try codeAdd(b1);
    try codeAdd(b2);
}

fn emitLoop(loopStart: i32) !void {
    try emitOp(Op.Loop);

    var offset = codeLen() - loopStart + 2;
    if (offset > math.maxInt(u16) + 1) errored("Loop body too large");

    var o = @intCast(u16, offset);
    try emitByte(@intCast(u8, (o >> 8) & 0xFF));
    try emitByte(@intCast(u8, o & 0xFF));
}

fn emitJump(instr: Op) !i32 {
    try emitOp(instr);
    try emitBytes(0xFF, 0xFF);
    return @intCast(i32, codeLen()) - 2;
}

fn emitReturn() !void {
    try emitOp(Op.Nil);
    try emitOp(Op.Return);
}

fn makeConstant(value: Value) !u8 {
    var constant = try currentChunk().addConstant(value);
    if (constant > math.maxInt(u8)) {
        errored("Too many constants in one chunk.");
        return 0;
    }
    return @intCast(u8, constant);
}

fn emitConstant(value: Value) !void {
    var constant = try makeConstant(value);
    try emitOp(Op.Constant);
    try emitByte(constant);
}

fn patchJump(offset: i32) void {
    var jump = codeLen() - offset - 2;

    if (jump > math.maxInt(u8)) {
        errored("Too much code to jump over.");
    }

    codeSet(offset, @intCast(u8, (jump >> 8) & 0xFF));
    codeSet(offset + 1, @intCast(u8, jump & 0xFF));
}

fn endCompiler() !*ObjFunction {
    try emitReturn();
    var func = current.?.func;

    if (vm.DEBUG_PRINT) {
        if (!parser.hadError) {
            const name = if (func.name != null) func.name.?.chars else "<script>";
            try debug.disassembleChunk(currentChunk(), name);
        }
    }

    current = current.?.enclosing;
    return func;
}

fn beginScope() void {
    current.?.scopeDepth += 1;
}

fn endScope() !void {
    current.?.scopeDepth -= 1;

    while (current.?.localCount > 0 and current.?.locals[@intCast(usize, current.?.localCount - 1)].depth > current.?.scopeDepth) {
        if (current.?.locals[@intCast(usize, current.?.localCount) - 1].isCaptured) {
            try emitOp(Op.CloseUpvalue);
        } else {
            try emitOp(Op.Pop);
        }
        current.?.localCount -= 1;
    }
}

fn identifierConstant(name: *const Token) !u8 {
    var str = try ObjString.copy(current.?.allocator, name.chars);
    return makeConstant(Value{ .obj = &str.obj });
}

fn identifiersEqual(a: *const Token, b: *const Token) bool {
    return std.mem.eql(u8, a.chars, b.chars);
}

fn resolveLocal(compiler: *Compiler, name: *const Token) i32 {
    var i = compiler.localCount - 1;
    while (i >= 0) : (i -= 1) {
        var local: *Local = &compiler.locals[@intCast(usize, i)];
        if (identifiersEqual(name, &local.name)) {
            return i;
        }
    }
    return -1;
}

fn addUpvalue(compiler: *Compiler, index: u8, isLocal: bool) !i32 {
    for (compiler.upvalues.items) |upval, i| {
        if (upval.index == index and upval.isLocal == isLocal) {
            return @intCast(i32, i);
        }
    }

    if (compiler.func.upvalueCount == std.math.maxInt(u8)) {
        errored("Too many closure variables in function.");
        return 0;
    }

    try compiler.upvalues.append(Upvalue.init(index, isLocal));

    compiler.func.upvalueCount += 1;
    return compiler.func.upvalueCount - 1;
}

fn resolveUpvalue(compiler: *Compiler, name: *Token) anyerror!i32 {
    if (compiler.enclosing == null) return -1;

    var local = resolveLocal(compiler.enclosing.?, name);
    if (local != -1) {
        compiler.enclosing.?.locals[@intCast(usize, local)].isCaptured = true;
        return try addUpvalue(compiler, @intCast(u8, local), true);
    }

    var upvalue = try resolveUpvalue(compiler.enclosing.?, name);
    if (upvalue != -1)
        return try addUpvalue(compiler, @intCast(u8, upvalue), false);

    return -1;
}

fn addLocal(name: Token) void {
    if (current.?.localCount >= math.maxInt(u8) + 1) {
        errored("Too many local variables in function.");
        return;
    }
    var local = &current.?.locals[@intCast(usize, current.?.localCount)];
    current.?.localCount += 1;
    local.* = Local{ .name = name, .depth = -1, .isCaptured = false };
}

fn declareVariable() void {
    if (current.?.scopeDepth == 0) return;

    var name = &parser.previous;
    var i = current.?.localCount - 1;
    while (i >= 0) : (i -= 1) {
        var local = current.?.locals[@intCast(usize, i)];
        if (local.depth != -1 and local.depth < current.?.scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local.name)) {
            errored("Variable with this name already declared in this scope.");
        }
    }
    addLocal(name.*);
}

fn parseVariable(errMsg: []const u8) !u8 {
    consume(TokenType.IDENTIFIER, errMsg);

    declareVariable();
    if (current.?.scopeDepth > 0) return @intCast(u8, 0);

    return identifierConstant(&parser.previous);
}

fn markInitialized() void {
    if (current.?.scopeDepth == 0) return;
    current.?.locals[@intCast(usize, current.?.localCount - 1)].depth = current.?.scopeDepth;
}

fn defineVariable(global: u8) !void {
    if (current.?.scopeDepth > 0) {
        markInitialized();
        return;
    }
    try emitOp(Op.DefineGlobal);
    try emitByte(global);
}

fn or_(canAssign: bool) !void {
    var elseJump = try emitJump(Op.JumpIfFalse);
    var endJump = try emitJump(Op.Jump);

    patchJump(elseJump);
    try emitOp(Op.Pop);

    try parsePrecedence(Precedence.Or);
    patchJump(endJump);
}

fn and_(canAssign: bool) !void {
    var endJump = try emitJump(Op.JumpIfFalse);

    try emitOp(Op.Pop);
    try parsePrecedence(Precedence.And);

    patchJump(endJump);
}

fn binary(canAssign: bool) !void {
    var opType = parser.previous.typ;
    var rule = getRule(opType);
    try parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

    switch (opType) {
        TokenType.BANG_EQUAL => try emitOps(Op.Equal, Op.Not),
        TokenType.EQUAL_EQUAL => try emitOp(Op.Equal),
        TokenType.GREATER => try emitOp(Op.Greater),
        TokenType.GREATER_EQUAL => try emitOps(Op.Less, Op.Not),
        TokenType.LESS => try emitOp(Op.Less),
        TokenType.LESS_EQUAL => try emitOps(Op.Greater, Op.Not),

        TokenType.PLUS => try emitOp(Op.Add),
        TokenType.MINUS => try emitOp(Op.Subtract),
        TokenType.STAR => try emitOp(Op.Multiply),
        TokenType.SLASH => try emitOp(Op.Divide),
        else => {},
    }
}

fn argumentList() !u8 {
    var argCount: u8 = 0;
    if (!check(TokenType.RIGHT_PAREN)) {
        var comma = true;
        while (comma) {
            try expression();

            argCount += 1;
            if (argCount == 255)
                errored("Cannot have more than 255 arguments.");

            comma = match(TokenType.COMMA);
        }
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

fn call(canAssign: bool) !void {
    var argCount = try argumentList();
    try emitOp(Op.Call);
    try emitByte(argCount);
}

fn literal(canAssign: bool) !void {
    switch (parser.previous.typ) {
        TokenType.FALSE => try emitOp(Op.False),
        TokenType.NIL => try emitOp(Op.Nil),
        TokenType.TRUE => try emitOp(Op.True),
        else => {},
    }
}

fn grouping(canAssign: bool) !void {
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn number(canAssign: bool) !void {
    var num = try std.fmt.parseFloat(f64, parser.previous.chars);
    try emitConstant(Value{ .number = num });
}

fn string(canAssign: bool) !void {
    var str = try ObjString.copy(current.?.allocator, parser.previous.chars[1 .. parser.previous.chars.len - 1]);
    try emitConstant(Value{ .obj = &str.obj });
}

fn namedVariable(name: Token, canAssign: bool) !void {
    var myName = name;
    var getOp: Op = undefined;
    var setOp: Op = undefined;
    var arg = resolveLocal(current.?, &myName);
    if (arg != -1) {
        getOp = Op.GetLocal;
        setOp = Op.SetLocal;
    } else {
        arg = try resolveUpvalue(current.?, &myName);
        if (arg != -1) {
            getOp = Op.GetUpvalue;
            setOp = Op.SetUpvalue;
        } else {
            arg = try identifierConstant(&myName);
            getOp = Op.GetGlobal;
            setOp = Op.SetGlobal;
        }
    }

    if (canAssign and match(TokenType.EQUAL)) {
        try expression();
        try emitOp(setOp);
        try emitByte(@intCast(u8, arg));
    } else {
        try emitOp(getOp);
        try emitByte(@intCast(u8, arg));
    }
}

fn variable(canAssign: bool) !void {
    try namedVariable(parser.previous, canAssign);
}

fn unary(canAssign: bool) !void {
    var opType = parser.previous.typ;

    try parsePrecedence(Precedence.Unary);

    switch (opType) {
        TokenType.BANG => try emitOp(Op.Not),
        TokenType.MINUS => try emitOp(Op.Negate),
        else => {},
    }
}

// zig fmt: off
const rules = [_]ParseRule {
    ParseRule.init(grouping, call,    Precedence.Call),       // TOKEN_LEFT_PAREN      
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_RIGHT_PAREN     
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_LEFT_BRACE
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_RIGHT_BRACE     
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_COMMA           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_DOT             
    ParseRule.init(unary,    binary,  Precedence.Term),       // TOKEN_MINUS           
    ParseRule.init(null,     binary,  Precedence.Term),       // TOKEN_PLUS            
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_SEMICOLON       
    ParseRule.init(null,     binary,  Precedence.Factor),     // TOKEN_SLASH           
    ParseRule.init(null,     binary,  Precedence.Factor),     // TOKEN_STAR            
    ParseRule.init(unary,    null,    Precedence.None),       // TOKEN_BANG            
    ParseRule.init(null,     binary,  Precedence.Equality),   // TOKEN_BANG_EQUAL      
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_EQUAL           
    ParseRule.init(null,     binary,  Precedence.Equality),   // TOKEN_EQUAL_EQUAL     
    ParseRule.init(null,     binary,  Precedence.Comparison), // TOKEN_GREATER         
    ParseRule.init(null,     binary,  Precedence.Comparison), // TOKEN_GREATER_EQUAL   
    ParseRule.init(null,     binary,  Precedence.Comparison), // TOKEN_LESS            
    ParseRule.init(null,     binary,  Precedence.Comparison), // TOKEN_LESS_EQUAL      
    ParseRule.init(variable, null,    Precedence.None),       // TOKEN_IDENTIFIER      
    ParseRule.init(string,   null,    Precedence.None),       // TOKEN_STRING          
    ParseRule.init(number,   null,    Precedence.None),       // TOKEN_NUMBER          
    ParseRule.init(null,     and_,    Precedence.And),        // TOKEN_AND             
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_CLASS           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_ELSE            
    ParseRule.init(literal,  null,    Precedence.None),       // TOKEN_FALSE           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_FOR             
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_FUN             
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_IF              
    ParseRule.init(literal,  null,    Precedence.None),       // TOKEN_NIL             
    ParseRule.init(null,     or_,     Precedence.Or),         // TOKEN_OR              
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_PRINT           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_RETURN          
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_SUPER           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_THIS            
    ParseRule.init(literal,  null,    Precedence.None),       // TOKEN_TRUE            
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_VAR             
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_WHILE           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_ERROR           
    ParseRule.init(null,     null,    Precedence.None),       // TOKEN_EOF   
};
// zig fmt: on

fn parsePrecedence(precedence: Precedence) !void {
    advance();

    var canAssign = false;
    if (getRule(parser.previous.typ).prefix) |prefixRule| {
        canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try prefixRule(canAssign);
    } else {
        errored("Expect expression.");
        return;
    }

    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.typ).precedence)) {
        advance();
        if (getRule(parser.previous.typ).infix) |infixRule| {
            try infixRule(canAssign);
        }
    }

    if (canAssign and match(TokenType.EQUAL)) {
        errored("Invalid assignment target.");
    }
}

fn getRule(typ: TokenType) *const ParseRule {
    return &rules[@enumToInt(typ)];
}

fn expression() !void {
    try parsePrecedence(Precedence.Assignment);
}

fn block() anyerror!void {
    while (!check(TokenType.RIGHT_BRACE) and !check(TokenType.EOF)) {
        try declaration();
    }
    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

fn function(funcType: FunctionType) !void {
    var compiler = try Compiler.init(current.?.allocator, .Function);
    defer compiler.deinit();

    current = &compiler;

    beginScope();

    consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TokenType.RIGHT_PAREN)) {
        var comma = true;
        while (comma) {
            current.?.func.arity += 1;
            if (current.?.func.arity == 255) {
                errorCurrent("Cannot have more than 255 parameters.");
            }

            var paramConstant = try parseVariable("Expect parameter name");
            try defineVariable(paramConstant);

            comma = match(TokenType.COMMA);
        }
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");

    consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");
    try block();

    var func = try endCompiler();
    try emitOp(Op.Closure);
    try emitByte(try makeConstant(Value.fromObj(&func.obj)));

    var i: usize = 0;
    while (i < func.upvalueCount) : (i += 1) {
        try emitByte(@boolToInt(compiler.upvalues.items[i].isLocal));
        try emitByte(compiler.upvalues.items[i].index);
    }
}

fn funDeclaration() !void {
    var global = try parseVariable("Expect function name.");
    markInitialized();
    try function(FunctionType.Function);
    try defineVariable(global);
}

fn varDeclaration() !void {
    var global = try parseVariable("Expect variable name.");

    if (match(TokenType.EQUAL)) {
        try expression();
    } else {
        try emitOp(Op.Nil);
    }
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");

    try defineVariable(global);
}

fn expressionStatement() !void {
    try expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    try emitOp(Op.Pop);
}

fn ifStatement() !void {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    var thenJump = try emitJump(Op.JumpIfFalse);
    try emitOp(Op.Pop);
    try statement();

    var elseJump = try emitJump(Op.Jump);

    patchJump(thenJump);
    try emitOp(Op.Pop);

    if (match(TokenType.ELSE)) try statement();
    patchJump(elseJump);
}

fn printStatement() !void {
    try expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    try emitOp(Op.Print);
}

fn returnStatement() !void {
    if (current.?.funcType == .Script) {
        errored("Cannot return from top-level code.");
    }

    if (match(TokenType.SEMICOLON)) {
        try emitReturn();
    } else {
        try expression();
        consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        try emitOp(Op.Return);
    }
}

fn whileStatement() !void {
    var loopStart = codeLen();

    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'");
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition");

    var exitJump = try emitJump(Op.JumpIfFalse);

    try emitOp(Op.Pop);
    try statement();

    try emitLoop(loopStart);

    patchJump(exitJump);
    try emitOp(Op.Pop);
}

// zig fmt: off
fn synchronize() void {
    parser.panicMode = false;

    while (parser.current.typ != TokenType.EOF) {
        if (parser.previous.typ == TokenType.SEMICOLON) return;
        switch (parser.current.typ) {
            TokenType.CLASS,
            TokenType.FUN,
            TokenType.VAR,
            TokenType.FOR,
            TokenType.IF,
            TokenType.WHILE,
            TokenType.PRINT,
            TokenType.RETURN => return,
            else => {}
        }

        advance();
    }
}
// zig fmt: on

fn declaration() !void {
    if (match(TokenType.FUN)) {
        try funDeclaration();
    } else if (match(TokenType.VAR)) {
        try varDeclaration();
    } else {
        try statement();
    }
    if (parser.panicMode) synchronize();
}

fn statement() anyerror!void {
    if (match(TokenType.PRINT)) {
        try printStatement();
    } else if (match(TokenType.IF)) {
        try ifStatement();
    } else if (match(TokenType.RETURN)) {
        try returnStatement();
    } else if (match(TokenType.WHILE)) {
        try whileStatement();
    } else if (match(TokenType.LEFT_BRACE)) {
        beginScope();
        try block();
        try endScope();
    } else {
        try expressionStatement();
    }
}

pub fn compile(allocator: *Allocator, source: []const u8) !*ObjFunction {
    scanner.initScanner(source);

    var compiler = try Compiler.init(allocator, .Script);
    defer compiler.deinit();
    current = &compiler;

    parser = Parser{};

    advance();

    while (!match(TokenType.EOF)) {
        try declaration();
    }

    return if (parser.hadError)
        InterpretError.Compile
    else
        try endCompiler();
}

pub fn markCompilerRoots() !void {
    var comp: ?*Compiler = current;

    while (comp != null) {
        try object.markObject(&comp.?.func.obj);
        comp = comp.?.enclosing;
    }
}
