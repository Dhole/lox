const std = @import("std");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Allocator = std.mem.Allocator;

const interpreter = @import("interpreter.zig");

const Interpreter = interpreter.Interpreter;

pub const Resolver = struct {
    const Self = @This();

    allocator: *Allocator,
    int: *Interpreter,
    scopes: ArrayList(StringHashMap(bool)),

    pub fn init(int: *Interpreter, allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .int = int,
            .scopes = try ArrayList(StringHashMap(bool)).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // TODO: free each scope
        self.scopes.deinit();
    }

    fn beginScope(self: *Self) !void {
        try self.scopes.push(StringHashMap(bool).init(self.allocator));
    }

    fn endScope(self: *Self) void {
        var scope = self.scopes.pop();
        scope.deinit();
    }

    fn resolveExpr(self: *Self, exp: *const Expr) !void {
        switch (exp.*) {
            else => @panic("unimplemented"),
        }
    }

    fn resolveStmt(self: *Self, stm: *const Stmt) !void {
        switch (stm.*) {
            .block => |stms| {
                self.beginScope();
                try self.resolve(stms);
                self.endScope();
            },
            else => @panic("unimplemented"),
        }
    }

    pub fn resolve(self: *Self, stms: []const Stmt) !void {
        for (stms) |*stm| {
            self.resolveStmt(stm);
        }
    }
};
