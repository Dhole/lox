const std = @import("std");

const token = @import("token.zig");
const scanner = @import("scanner.zig");
const expr = @import("expr.zig");
const parser = @import("parser.zig");
const helpers = @import("helpers.zig");
const interpreter = @import("interpreter.zig");

const Expr = expr.Expr;
const Parser = parser.Parser;
const Interpreter = interpreter.Interpreter;

const MAX_FILE_SIZE = 0x1000000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = &gpa.allocator;
// var allocator = std.testing.allocator;

pub fn main() !u8 {
    // defer {
    //     _ = std.testing.allocator_instance.deinit();
    // }
    const args = std.os.argv[1..std.os.argv.len];
    var m = try Main.init();
    defer m.deinit();
    if (args.len > 1) {
        std.log.err("Usage: jlox [script]", .{});
        return 64;
    } else if (args.len == 1) {
        const arg0 = std.mem.sliceTo(args[0], '0');
        return try m.runFile(arg0);
    } else {
        return try m.runPrompt();
    }
    return 0;
}

const Main = struct {
    const Self = @This();

    interpreter: Interpreter,

    fn init() !Self {
        return Self{
            .interpreter = try Interpreter.init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.interpreter.deinit();
    }

    fn runFile(self: *Self, path: []u8) !u8 {
        std.log.info("path: {s}", .{path});

        var path_file = try std.fs.cwd().openFile(path, .{ .read = true });
        const bytes = try path_file.readToEndAlloc(allocator, MAX_FILE_SIZE);
        defer allocator.free(bytes);

        try self.run(bytes);

        if (helpers.hadError) {
            return 65;
        }
        if (helpers.hadRuntimeError) {
            return 70;
        }
        return 0;
    }

    fn runPrompt(self: *Self) !u8 {
        const stdout = std.io.getStdOut();
        const stdin = std.io.getStdIn();
        const reader = std.io.bufferedReader(stdin.reader()).reader();

        var buffer: [1024]u8 = undefined;
        while (true) {
            try stdout.writeAll("> ");
            const line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
            if (line) |l| {
                try self.run(l);
                helpers.hadError = false;
                helpers.hadRuntimeError = false;
            } else {
                return 0;
            }
        }
    }

    fn run(self: *Self, bytes: []u8) !void {
        // std.log.info("{any}", .{bytes});
        var s = try scanner.Scanner.init(allocator, bytes);
        defer s.deinit();
        const tokens = try s.scanTokens();

        var p = try Parser.init(allocator, &self.interpreter.funcArena, tokens);
        defer p.deinit();

        var statements = p.parse() catch |e| {
            switch (e) {
                error.ParseError => return,
                else => return e,
            }
        };

        if (helpers.hadError) {
            return;
        }

        var w = std.io.getStdOut().writer();
        try self.interpreter.interpret(w, statements.items);
    }
};
