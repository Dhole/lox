const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const helpers = @import("helpers.zig");

const Token = token.Token;
const TT = token.TokenType;
const Lit = token.Token.Literal;

pub const Scanner = struct {
    const Self = @This();

    allocator: *Allocator,
    keywords: std.StringHashMap(TT),
    source: []const u8,
    tokens: ArrayList(Token),
    start: u32,
    current: u32,
    line: u32,

    fn keywordsInit(allocator: *Allocator) !std.StringHashMap(TT) {
        var keywords = std.StringHashMap(TT).init(allocator);
        try keywords.put("and", TT.AND);
        try keywords.put("class", TT.CLASS);
        try keywords.put("else", TT.ELSE);
        try keywords.put("false", TT.FALSE);
        try keywords.put("for", TT.FOR);
        try keywords.put("fun", TT.FUN);
        try keywords.put("if", TT.IF);
        try keywords.put("nil", TT.NIL);
        try keywords.put("or", TT.OR);
        try keywords.put("print", TT.PRINT);
        try keywords.put("return", TT.RETURN);
        try keywords.put("super", TT.SUPER);
        try keywords.put("this", TT.THIS);
        try keywords.put("true", TT.TRUE);
        try keywords.put("var", TT.VAR);
        try keywords.put("while", TT.WHILE);
        return keywords;
    }

    pub fn init(allocator: *Allocator, source: []const u8) !Self {
        const keywords = try keywordsInit(allocator);
        return Self{
            .allocator = allocator,
            .keywords = keywords,
            .source = source,
            .tokens = ArrayList(Token).init(allocator),
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn deinit(self: *Self) void {
        self.keywords.deinit();
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Self) !ArrayList(Token) {
        while (!self.isAtEnd()) {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(Token.init(TT.EOF, "", null, self.line));
        return self.tokens;
    }

    fn scanToken(self: *Self) !void {
        const c = self.advance();
        const token_type = switch (c) {
            '(' => TT.LEFT_PAREN,
            ')' => TT.RIGHT_PAREN,
            '{' => TT.LEFT_BRACE,
            '}' => TT.RIGHT_BRACE,
            ',' => TT.COMMA,
            '.' => TT.DOT,
            '-' => TT.MINUS,
            '+' => TT.PLUS,
            ';' => TT.SEMICOLON,
            '*' => TT.STAR,
            '!' => if (self.match('=')) TT.BANG_EQUAL else TT.BANG,
            '=' => if (self.match('=')) TT.EQUAL_EQUAL else TT.EQUAL,
            '<' => if (self.match('=')) TT.LESS_EQUAL else TT.LESS,
            '>' => if (self.match('=')) TT.GREATER_EQUAL else TT.GREATER,
            '/' => blk: {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                    return;
                } else {
                    break :blk TT.SLASH;
                }
            },
            '"' => {
                try self.string();
                return;
            },
            '0'...'9' => {
                try self.number();
                return;
            },
            'a'...'z', 'A'...'Z', '_' => {
                try self.identifier();
                return;
            },
            // Ignore whitespace
            ' ' => return,
            '\r' => return,
            '\t' => return,
            '\n' => {
                self.line += 1;
                return;
            },
            else => {
                helpers.printLineErr(self.line, "Unexpected character.");
                return;
            },
        };
        try self.addToken(token_type);
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn peek(self: *Self) u8 {
        return if (self.isAtEnd()) '\x00' else self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        return if (self.current + 1 >= self.source.len) '\x00' else self.source[self.current + 1];
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

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            helpers.printLineErr(self.line, "Unternimated string.");
            return;
        }
        // The closing ".
        _ = self.advance();

        // Trim the surrounding quotes.
        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addTokenLit(TT.STRING, Lit{ .string = value });
    }

    fn number(self: *Self) !void {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for a fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "."
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        const num = try std.fmt.parseFloat(f64, self.source[self.start..self.current]);
        try self.addTokenLit(TT.NUMBER, Lit{ .number = num });
    }

    fn identifier(self: *Self) !void {
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }
        const text = self.source[self.start..self.current];
        const typ = if (self.keywords.get(text)) |t| t else TT.IDENTIFIER;
        try self.addToken(typ);
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c == '_');
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn addToken(self: *Self, typ: TT) !void {
        try self.addTokenLit(typ, null);
    }

    fn addTokenLit(self: *Self, typ: TT, literal: ?Lit) !void {
        const text = self.source[self.start..self.current];
        try self.tokens.append(Token.init(typ, text, literal, self.line));
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }
};

test "scanner" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var scanner = try Scanner.init(&gpa.allocator, "({});");
    defer scanner.deinit();
    _ = try scanner.scanTokens();
}
