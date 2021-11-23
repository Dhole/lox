const std = @import("std");

const _token = @import("token.zig");

const Token = _token.Token;
const TT = _token.TokenType;

pub const Scanner = struct {
    const Self = @This();

    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) {
            return self.makeToken(TT.EOF);
        }
        const c = self.advance();
        if (isDigit(c)) {
            return self.number();
        }
        if (isAlpha(c)) {
            return self.identifier();
        }

        switch (c) {
            '(' => return self.makeToken(TT.LEFT_PAREN),
            ')' => return self.makeToken(TT.RIGHT_PAREN),
            '{' => return self.makeToken(TT.LEFT_BRACE),
            '}' => return self.makeToken(TT.RIGHT_BRACE),
            ';' => return self.makeToken(TT.SEMICOLON),
            ',' => return self.makeToken(TT.COMMA),
            '.' => return self.makeToken(TT.DOT),
            '-' => return self.makeToken(TT.MINUS),
            '+' => return self.makeToken(TT.PLUS),
            '/' => return self.makeToken(TT.SLASH),
            '*' => return self.makeToken(TT.STAR),
            '!' => return self.makeToken(if (self.match('=')) TT.BANG_EQUAL else TT.BANG),
            '=' => return self.makeToken(if (self.match('=')) TT.EQUAL_EQUAL else TT.EQUAL),
            '<' => return self.makeToken(if (self.match('=')) TT.LESS_EQUAL else TT.LESS),
            '>' => return self.makeToken(if (self.match('=')) TT.GREATER_EQUAL else TT.GREATER),
            '"' => return self.string(),
            else => {},
        }

        return self.errorToken("Unexpected character.");
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }
        // The closing quote
        _ = self.advance();

        return self.makeToken(TT.STRING);
    }

    fn number(self: *Self) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        // Look for a fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the ".".
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(TT.NUMBER);
    }

    fn identifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Self) TT {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", TT.AND),
            'c' => return self.checkKeyword(1, "lass", TT.CLASS),
            'e' => return self.checkKeyword(1, "lse", TT.ELSE),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'a' => return self.checkKeyword(2, "lse", TT.FALSE),
                        'o' => return self.checkKeyword(2, "r", TT.FOR),
                        'u' => return self.checkKeyword(2, "n", TT.FUN),
                        else => {},
                    }
                }
            },
            'i' => return self.checkKeyword(1, "f", TT.IF),
            'n' => return self.checkKeyword(1, "il", TT.NIL),
            'o' => return self.checkKeyword(1, "r", TT.OR),
            'p' => return self.checkKeyword(1, "rint", TT.PRINT),
            'r' => return self.checkKeyword(1, "eturn", TT.RETURN),
            's' => return self.checkKeyword(1, "uper", TT.SUPER),
            't' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'h' => return self.checkKeyword(2, "is", TT.THIS),
                        'r' => return self.checkKeyword(2, "ue", TT.TRUE),
                        else => {},
                    }
                }
            },
            'v' => return self.checkKeyword(1, "ar", TT.VAR),
            'w' => return self.checkKeyword(1, "hile", TT.WHILE),
            else => {},
        }
        return TT.IDENTIFIER;
    }

    fn checkKeyword(self: *Self, start: usize, rest: []const u8, typ: TT) TT {
        if (std.mem.eql(u8, self.source[self.start + start .. self.start + start + rest.len], rest)) {
            return typ;
        }
        return TT.IDENTIFIER;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isAtEnd(self: *Self) bool {
        return self.current == self.source.len;
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.isAtEnd() or self.current + 1 == self.source.len) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[self.current] != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn makeToken(self: *Self, typ: TT) Token {
        return Token{
            .type = typ,
            .value = self.source[self.start..self.current],
            // .start = self.start,
            // .length = self.current - self.start,
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, message: []const u8) Token {
        return Token{
            .type = TT.ERROR,
            .value = message,
            .line = self.line,
        };
    }
};
