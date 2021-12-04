pub const OpCode = enum {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    RETURN,
};

pub const Flags = struct {
    debugTraceExecution: bool,
    debugPrintCode: bool,
};
