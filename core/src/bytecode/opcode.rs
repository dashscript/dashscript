macro_rules! write_main_opcodes {
    ($($name:ident = $byte:expr)+) => {
        $(pub const $name: u8 = $byte;)+

        pub fn to_string(opcode: u8) -> String {
            match opcode {
                $($byte => stringify!($name).to_owned(),)+
                _ => "UNKNOWN".to_owned()
            }
        }
    };
}

macro_rules! write_opcodes {
    ($($name:ident = $byte:expr)+) => {
        $(pub const $name: u8 = $byte;)+
    };
}

// Actual operation codes
write_main_opcodes!(
    // Opcodes to load basic constants
    FALSE = 0
    TRUE = 1
    NULL = 3

    // Opcodes which is used to manage memory
    GET_LOCAL = 4
    GET_GLOBAL = 5
    GET_UPVALUE = 6
    GET_ATTR = 7
    SET_LOCAL = 8
    SET_GLOBAL = 9
    SET_ATTR = 10
    SET_UPVALUE = 47

    // Opcodes for basic arithmetic operations
    ADD = 11
    SUB = 12
    MUL = 13
    DIV = 14
    REM = 15
    BITAND = 16
    BITOR = 48
    BITXOR = 49
    SHR = 50
    SHL = 51
    
    // Opcodes to load constants
    STRING = 17
    STRING_LONG = 18
    INT = 19
    INT_LONG = 20
    FLOAT = 21
    FLOAT_LONG = 22

    // Opcodes for tack based operation
    POP = 23
    
    // Opcodes to perform binary operations
    AND = 24
    EQ = 25
    NEQ = 26
    GT = 27
    GTE = 28
    LT = 29
    LTE = 30

    // Opcodes to perform unary operation
    NOT = 32

    // --
    CALL = 33
    CALL_CHILD = 34
    ARRAY = 35
    DICT = 36
    FUNC = 37

    // --
    RETURN = 38
    ITER = 39
    ITER_NEXT = 40

    // Opcodes to change ip of the vm
    JUMP = 41
    JUMP_BACK = 42
    JUMP_IF = 43
    JUMP_NOT_IF = 44

    // Other bytecodes
    IMPORT = 45
    CLOSE_UPVALUE = 46

    // Dead bytecodes 
    POW = 80
);

// Assignment operation codes
write_opcodes!(
    ASSIGN_OP = 0
    ASSIGN_OP_ADD = 1
    ASSIGN_OP_SUB = 2
);

// Long and short opcodes
write_opcodes!(
    OP_SHORT = 0
    OP_LONG = 1
);