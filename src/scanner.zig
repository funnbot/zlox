const std = @import("std");
const ascii = std.ascii;

pub const TokenType = enum {
// Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER, STRING, NUMBER,

    // Keywords.
    AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, ERROR, EOF
};

pub const Token = struct {
    typ: TokenType,
    chars: []const u8,
    line: usize,
};

const Scanner = struct {
    source: []const u8,
    start: [*]const u8,
    current: [*]const u8,
    line: usize,
};

var scanner: Scanner = undefined;

pub fn initScanner(source: []const u8) void {
    scanner = Scanner{
        .source = source,
        .start = source.ptr,
        .current = source.ptr,
        .line = 1,
    };
}

fn currentLen() usize {
    return @ptrToInt(scanner.current) - @ptrToInt(scanner.start);
}

fn isAtEnd() bool {
    return @ptrToInt(scanner.current) >= @ptrToInt(scanner.source.ptr) + scanner.source.len;
}

fn advance() u8 {
    scanner.current += 1;
    return (scanner.current - 1)[0];
}

fn peek() u8 {
    return scanner.current[0];
}

fn peekNext() u8 {
    if (isAtEnd()) return 0;
    return (scanner.current + 1)[0];
}

fn match(expected: u8) bool {
    if (isAtEnd()) return false;
    if (scanner.current[0] != expected) return false;

    scanner.current += 1;
    return true;
}

fn makeToken(typ: TokenType) Token {
    return Token{
        .typ = typ,
        .chars = scanner.start[0..currentLen()],
        .line = scanner.line,
    };
}

fn errorToken(msg: []const u8) Token {
    return Token{
        .typ = TokenType.ERROR,
        .chars = msg,
        .line = scanner.line,
    };
}

fn skipWhitespace() void {
    while (true) {
        switch (peek()) {
            ' ', '\r', '\t' => _ = advance(),
            '\n' => {
                scanner.line += 1;
                _ = advance();
            },
            '/' => {
                if (peekNext() == '/') {
                    while (peek() != '\n' and !isAtEnd()) _ = advance();
                } else break;
            },
            else => break,
        }
    }
}

fn checkKeyword(start: usize, length: usize, rest: []const u8, typ: TokenType) TokenType {
    var wordRest = scanner.start[start .. start + length];
    if ((currentLen() == start + length) and std.mem.eql(u8, wordRest, rest)) {
        return typ;
    }

    return TokenType.IDENTIFIER;
}

fn identifierType() TokenType {
    return switch (scanner.start[0]) {
        'a' => checkKeyword(1, 2, "nd", TokenType.AND),
        'c' => checkKeyword(1, 4, "lass", TokenType.CLASS),
        'e' => checkKeyword(1, 3, "lse", TokenType.ELSE),
        'f' => {
            if (currentLen() <= 1) return TokenType.IDENTIFIER;
            return switch (scanner.start[1]) {
                'a' => checkKeyword(2, 3, "lse", TokenType.FALSE),
                'o' => checkKeyword(2, 1, "r", TokenType.FOR),
                'u' => checkKeyword(2, 1, "n", TokenType.FUN),
                else => TokenType.IDENTIFIER,
            };
        },
        'i' => checkKeyword(1, 1, "f", TokenType.IF),
        'n' => checkKeyword(1, 2, "il", TokenType.NIL),
        'o' => checkKeyword(1, 1, "r", TokenType.OR),
        'p' => checkKeyword(1, 4, "rint", TokenType.PRINT),
        'r' => checkKeyword(1, 5, "eturn", TokenType.RETURN),
        's' => checkKeyword(1, 4, "uper", TokenType.SUPER),
        't' => {
            if (currentLen() <= 1) return TokenType.IDENTIFIER;
            return switch (scanner.start[1]) {
                'h' => checkKeyword(2, 2, "is", TokenType.THIS),
                'r' => checkKeyword(2, 2, "ue", TokenType.TRUE),
                else => TokenType.IDENTIFIER,
            };
        },
        'v' => checkKeyword(1, 2, "ar", TokenType.VAR),
        'w' => checkKeyword(1, 4, "hile", TokenType.WHILE),
        else => TokenType.IDENTIFIER,
    };
}

fn identifier() Token {
    while (ascii.isAlpha(peek()) or ascii.isDigit(peek())) _ = advance();

    return makeToken(identifierType());
}

fn number() Token {
    while (ascii.isDigit(peek())) _ = advance();

    if (peek() == '.' and ascii.isDigit(peekNext())) {
        _ = advance();
        while (ascii.isDigit(peek())) _ = advance();
    }

    return makeToken(TokenType.NUMBER);
}

fn string() Token {
    while (peek() != '"' and !isAtEnd()) {
        if (peek() == '\n') scanner.line += 1;
        _ = advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    _ = advance();
    return makeToken(TokenType.STRING);
}

pub fn scanToken() Token {
    skipWhitespace();
    scanner.start = scanner.current;
    if (isAtEnd()) return makeToken(TokenType.EOF);

    var c = advance();

    if (ascii.isAlpha(c)) return identifier();
    if (ascii.isDigit(c)) return number();

    return switch (c) {
        '(' => makeToken(TokenType.LEFT_PAREN),
        ')' => makeToken(TokenType.RIGHT_PAREN),
        '{' => makeToken(TokenType.LEFT_BRACE),
        '}' => makeToken(TokenType.RIGHT_BRACE),
        ';' => makeToken(TokenType.SEMICOLON),
        ',' => makeToken(TokenType.COMMA),
        '.' => makeToken(TokenType.DOT),
        '-' => makeToken(TokenType.MINUS),
        '+' => makeToken(TokenType.PLUS),
        '/' => makeToken(TokenType.SLASH),
        '*' => makeToken(TokenType.STAR),
        '!' => makeToken(if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
        '=' => makeToken(if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
        '<' => makeToken(if (match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
        '>' => makeToken(if (match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
        '"' => string(),
        else => errorToken("Unexpected token."),
    };
}
