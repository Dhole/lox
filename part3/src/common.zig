pub const OpCode = enum {
    CONSTANT,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NEGATE,
    RETURN,
};

pub const Flags = struct {
    debugTraceExecution: bool,
    debugPrintCode: bool,
};
