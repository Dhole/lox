const std = @import("std");

pub const TokenType = enum {
// Single-character tokens.
LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

// Literals.
IDENTIFIER, STRING, NUMBER,

// Keywords.
AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, EOF };

pub const Token = struct {
    pub const Literal = union(enum) {
        identifier: []const u8,
        string: []const u8,
        number: f64,
        none,
    };

    type: TokenType,
    lexeme: []const u8,
    literal: Literal,
    line: u32,

    pub fn init(typ: TokenType, lexeme: []const u8, literal: Literal, line: u32) @This() {
        return @This(){
            .type = typ,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{{type:{any}, lexeme:{s}, literal:", .{ self.type, self.lexeme });
        switch (self.literal) {
            .identifier => |*v| try writer.print("{s}", .{v.*}),
            .string => |*v| try writer.print("{s}", .{v.*}),
            .number => |*v| try writer.print("{d}", .{v.*}),
            .none => |_| {},
        }
        try writer.print("}}", .{});
    }
};

test "foo" {
    var token = Token.init(TokenType.IDENTIFIER, "var", Token.Literal{ .identifier = "var" }, 42);
    std.log.debug("{s}", .{token});
}
