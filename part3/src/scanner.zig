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
                    scanner.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (peek() != '\n' and !isAtEnd()) {
                            selfadvance();
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
            return self.makeToken(TT.TOKEN_EOF);
        }
        const c = self.advance();
        if (self.isDigit(c)) {
            return self.number();
        }
        if (self.isAlpha(c)) {
            return self.identifier();
        }

        switch (c) {
            '(' => return self.makeToken(TT.TOKEN_LEFT_PAREN),
            ')' => return self.makeToken(TT.TOKEN_RIGHT_PAREN),
            '{' => return self.makeToken(TT.TOKEN_LEFT_BRACE),
            '}' => return self.makeToken(TT.TOKEN_RIGHT_BRACE),
            ';' => return self.makeToken(TT.TOKEN_SEMICOLON),
            ',' => return self.makeToken(TT.TOKEN_COMMA),
            '.' => return self.makeToken(TT.TOKEN_DOT),
            '-' => return self.makeToken(TT.TOKEN_MINUS),
            '+' => return self.makeToken(TT.TOKEN_PLUS),
            '/' => return self.makeToken(TT.TOKEN_SLASH),
            '*' => return self.makeToken(TT.TOKEN_STAR),
            '!' => return self.makeToken(if (self.match('=')) TT.TOKEN_BANG_EQUAL else TT.TOKEN_BANG),
            '=' => return self.makeToken(if (self.match('=')) TT.TOKEN_EQUAL_EQUAL else TT.TOKEN_EQUAL),
            '<' => return self.makeToken(if (self.match('=')) TT.TOKEN_LESS_EQUAL else TT.TOKEN_LESS),
            '>' => return self.makeToken(if (self.match('=')) TT.TOKEN_GREATER_EQUAL else TT.TOKEN_GREATER),
            '"' => return self.string(),
        }

        return self.errorToken("Unexpected character.");
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                scanner.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }
        // The closing quote
        _ = self.advance();

        return self.makeToken(TT.TOKEN_STRING);
    }

    fn number(self: *Self) Token {
        while (self.isDigit(self.peek())) {
            _ = self.advance();
        }
        // Look for a fractional part.
        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            // Consume the ".".
            _ = self.advance();

            while (self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(TT.TOKEN_NUMBER);
    }

    fn identifier(self: *Self) Token {
        while (self.isAlpha(self.peek()) or self.isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Self) TokenType {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", TT.TOKEN_AND),
            'c' => return self.checkKeyword(1, "lass", TT.TOKEN_CLASS),
            'e' => return self.checkKeyword(1, "lse", TT.TOKEN_ELSE),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'a' => return checkKeyword(2, "lse", TOKEN_FALSE),
                        'o' => return checkKeyword(2, "r", TOKEN_FOR),
                        'u' => return checkKeyword(2, "n", TOKEN_FUN),
                    }
                }
            },
            'i' => return self.checkKeyword(1, "f", TT.TOKEN_IF),
            'n' => return self.checkKeyword(1, "il", TT.TOKEN_NIL),
            'o' => return self.checkKeyword(1, "r", TT.TOKEN_OR),
            'p' => return self.checkKeyword(1, "rint", TT.TOKEN_PRINT),
            'r' => return self.checkKeyword(1, "eturn", TT.TOKEN_RETURN),
            's' => return self.checkKeyword(1, "uper", TT.TOKEN_SUPER),
            't' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'h' => return checkKeyword(2, "is", TT.TOKEN_THIS),
                        'r' => return checkKeyword(2, "ue", TT.TOKEN_TRUE),
                    }
                }
            },
            'v' => return self.checkKeyword(1, "ar", TT.TOKEN_VAR),
            'w' => return self.checkKeyword(1, "hile", TT.TOKEN_WHILE),
            else => {},
        }
        return TT.TOKEN_IDENTIFIER;
    }

    fn checkKeyword(self: *Self, start: usize, rest: []const u8, typ: TokenType) TokenType {
        if (self.source[self.start .. self.start + rest.len] == rest) {
            return typ;
        }
        return TOKEN_IDENTIFIER;
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
        return self.source[self.current] == '\0';
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Self) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) {
            return '\0';
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

    fn makeToken(self: *Self, typ: TokenType) Token {
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
            .type = TT.TOKEN_ERROR,
            .value = message,
            .line = self.line,
        };
    }
};
