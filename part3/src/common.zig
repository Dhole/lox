pub const OpCode = enum {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    PRINT,
    RETURN,
};

pub const Flags = struct {
    debugTraceExecution: bool,
    debugPrintCode: bool,
};
