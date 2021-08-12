const std = @import("std");

const FmtOpts = std.fmt.FormatOptions;

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
    const Self = @This();

    pub const Literal = union(enum) {
        identifier: []const u8,
        string: []const u8,
        number: f64,
    };

    type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: u32,

    pub fn init(typ: TokenType, lexeme: []const u8, literal: ?Literal, line: u32) Self {
        return Self{
            .type = typ,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, opts: FmtOpts, w: anytype) !void {
        _ = opts;
        _ = fmt;
        try w.print("{{line: {d}, type:{any}, lexeme:{s}, literal:", .{ self.line, self.type, self.lexeme });
        if (self.literal) |lit| {
            switch (lit) {
                .identifier => |*v| try w.print("{s}", .{v.*}),
                .string => |*v| try w.print("{s}", .{v.*}),
                .number => |*v| try w.print("{d}", .{v.*}),
            }
        } else {
            try w.print("null", .{});
        }
        try w.print("}}", .{});
    }
};

test "foo" {
    var token = Token.init(TokenType.IDENTIFIER, "var", Token.Literal{ .identifier = "var" }, 42);
    std.log.debug("{s}", .{token});
}
