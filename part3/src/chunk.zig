pub const OpCode = enum {
    OP_RETURN,
};

pub const Chunk = struct {
    const Self = @This();

    count: usize,
    // capacity: usize, // capacity is code.len
    code: []u8,

    pub fn init() Self {
        return Self{
            .count = 0,
            .code = {},
        };
    }

    pub fn write(self: *Self) void {
        // TODO
    }
};
