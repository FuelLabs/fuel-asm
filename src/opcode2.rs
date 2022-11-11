/// Generates the following:
///
/// - A unique type for each opcode instruction type.
/// - Register and immediate value access methods for each opcode instruction type.
/// - An enum over all possible opcodes.
/// - An enum over all possible instructions.
macro_rules! impl_opcodes {
    // Define a unique struct for each opcode.
    (define_opcode_struct $doc:literal $ix:literal $Op:ident ($($field:ident),*), $($rest:tt)*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[doc = $doc]
        pub struct $Op([u8; 3]);
        impl_opcodes!(define_opcode_struct $($rest)*);
    };
    (define_opcode_struct) => {};

    // Implement accessors for register and immediate values.
    (define_opcode_methods $doc:literal $ix:literal $Op:ident ($($field:ident),*), $($rest:tt)*) => {
        impl $Op {
            /// The associated 6-bit Opcode value.
            pub const OPCODE: Opcode = Opcode::$Op;
        }
        impl_opcodes!(define_opcode_methods $($rest)*);
    };
    (define_opcode_methods) => {};

    // Define the `OpcodeRepr` enum.
    (define_opcode_enum $($doc:literal $ix:literal $Op:ident ($($field:ident),*),)*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[repr(u8)]
        pub enum Opcode {
            $(
                #[doc = $doc]
                $Op = $ix,
            )*
        }

        impl std::convert::TryFrom<u8> for Opcode {
            type Error = InvalidOpcode;
            fn try_from(u: u8) -> Result<Self, Self::Error> {
                match u {
                    $(
                        $ix => Ok(Opcode::$Op),
                    )*
                    _ => Err(InvalidOpcode),
                }
            }
        }
    };

    // Define the `Opcode` enum.
    (define_instruction_enum $($doc:literal $ix:literal $Op:ident ($($field:ident),*),)*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum Instruction {
            $(
                #[doc = $doc]
                $Op($Op),
            )*
        }
    };

    // Entrypoint to the macro, generates struct, methods, enum and instruction enum separately.
    ($($tts:tt)*) => {
        impl_opcodes!(define_opcode_struct $($tts)*);
        impl_opcodes!(define_opcode_methods $($tts)*);
        impl_opcodes!(define_opcode_enum $($tts)*);
        impl_opcodes!(define_instruction_enum $($tts)*);
    };
}

/// Failed to parse a `u8` as a valid or non-reserved opcode.
#[derive(Debug)]
pub struct InvalidOpcode;

impl_opcodes! {
    "Adds two registers."
    0x10 ADD(RegisterId, RegisterId, RegisterId),
    "Bitwise ANDs two registers."
    0x11 AND(RegisterId, RegisterId, RegisterId),
    "Divides two registers."
    0x12 DIV(RegisterId, RegisterId, RegisterId),
    "Compares two registers for equality."
    0x13 EQ(RegisterId, RegisterId, RegisterId),
    "Raises one register to the power of another."
    0x14 EXP(RegisterId, RegisterId, RegisterId),
    "Compares two registers for greater-than."
    0x15 GT(RegisterId, RegisterId, RegisterId),
    "Compares two registers for less-than."
    0x16 LT(RegisterId, RegisterId, RegisterId),
    "The integer logarithm of a register."
    0x17 MLOG(RegisterId, RegisterId, RegisterId),
    "The integer root of a register."
    0x18 MROO(RegisterId, RegisterId, RegisterId),
    "Modulo remainder of two registers."
    0x19 MOD(RegisterId, RegisterId, RegisterId),
    "Copy from one register to another."
    0x1A MOVE(RegisterId, RegisterId),
    "Multiplies two registers."
    0x1B MUL(RegisterId, RegisterId, RegisterId),
    "Bitwise NOT a register."
    0x1C NOT(RegisterId, RegisterId),
    "Bitwise ORs two registers."
    0x1D OR(RegisterId, RegisterId, RegisterId),
    "Left shifts a register by a register."
    0x1E SLL(RegisterId, RegisterId, RegisterId),
    "Right shifts a register by a register."
    0x1F SRL(RegisterId, RegisterId, RegisterId),
    "Subtracts two registers."
    0x20 SUB(RegisterId, RegisterId, RegisterId),
    "Bitwise XORs two registers."
    0x21 XOR(RegisterId, RegisterId, RegisterId),

    "Return from context."
    0x24 RET(RegisterId),
    "Return from context with data."
    0x25 RETD(RegisterId, RegisterId),
    "Allocate a number of bytes from the heap."
    0x26 ALOC(RegisterId),
    "Clear a variable number of bytes in memory."
    0x27 MCL(RegisterId, RegisterId),
    "Copy a variable number of bytes in memory."
    0x28 MCP(RegisterId, RegisterId, RegisterId),
    "Compare bytes in memory."
    0x29 MEQ(RegisterId, RegisterId, RegisterId, RegisterId),
    "Get block header hash for height."
    0x2A BHSH(RegisterId, RegisterId),
    "Get current block height."
    0x2B BHEI(RegisterId),
    "Burn coins of the current contract's asset ID."
    0x2C BURN(RegisterId),
    "Call a contract."
    0x2D CALL(RegisterId, RegisterId, RegisterId, RegisterId),
    "Copy contract code for a contract."
    0x2E CCP(RegisterId, RegisterId, RegisterId, RegisterId),
    "Get code root of a contract."
    0x2F CROO(RegisterId, RegisterId),
    "Get code size of a contract."
    0x30 CSIZ(RegisterId, RegisterId),
    "Get current block proposer's address."
    0x31 CB(RegisterId),
    "Load a contract's code as executable."
    0x32 LDC(RegisterId, RegisterId, RegisterId),
    "Log an event."
    0x33 LOG(RegisterId, RegisterId, RegisterId, RegisterId),
    "Log data."
    0x34 LOGD(RegisterId, RegisterId, RegisterId, RegisterId),
    "Mint coins of the current contract's asset ID."
    0x35 MINT(RegisterId),
    "Halt execution, reverting state changes and returning a value."
    0x36 RVRT(RegisterId),
    "Clear a series of slots from contract storage."
    0x37 SCWQ(RegisterId, RegisterId, RegisterId),
    "Load a word from contract storage."
    0x38 SRW(RegisterId, RegisterId, RegisterId),
    "Load a series of 32 byte slots from contract storage."
    0x39 SRWQ(RegisterId, RegisterId, RegisterId, RegisterId),
    "Store a word in contract storage."
    0x3A SWW(RegisterId, RegisterId, RegisterId),
    "Store a series of 32 byte slots in contract storage."
    0x3B SWWQ(RegisterId, RegisterId, RegisterId, RegisterId),
    "Transfer coins to a contract unconditionally."
    0x3C TR(RegisterId, RegisterId, RegisterId),
    "Transfer coins to a variable output."
    0x3D TRO(RegisterId, RegisterId, RegisterId, RegisterId),
    "The 64-byte public key (x, y) recovered from 64-byte signature on 32-byte message."
    0x3E ECR(RegisterId, RegisterId, RegisterId),
    "The keccak-256 hash of a slice."
    0x3F K256(RegisterId, RegisterId, RegisterId),
    "The SHA-2-256 hash of a slice."
    0x40 S256(RegisterId, RegisterId, RegisterId),
    "Get timestamp of block at given height."
    0x41 TIME(RegisterId, RegisterId),

    "Performs no operation."
    0x47 NOOP(),
    "Set flag register to a register."
    0x48 FLAG(RegisterId),
    "Get the balance of contract of an asset ID."
    0x49 BAL(RegisterId, RegisterId, RegisterId),
    "Dynamic jump."
    0x4A JMP(RegisterId),
    "Conditional dynamic jump."
    0x4B JNE(RegisterId, RegisterId, RegisterId),
    "Send a message to recipient address with call abi, coins, and output."
    0x4C SMO(RegisterId, RegisterId, RegisterId, RegisterId),
    "Adds a register and an immediate value."
    0x50 ADDI(RegisterId, RegisterId, Immediate12),
    "Bitwise ANDs a register and an immediate value."
    0x51 ANDI(RegisterId, RegisterId, Immediate12),
    "Divides a register and an immediate value."
    0x52 DIVI(RegisterId, RegisterId, Immediate12),
    "Raises one register to the power of an immediate value."
    0x53 EXPI(RegisterId, RegisterId, Immediate12),
    "Modulo remainder of a register and an immediate value."
    0x54 MODI(RegisterId, RegisterId, Immediate12),
    "Multiplies a register and an immediate value."
    0x55 MULI(RegisterId, RegisterId, Immediate12),
    "Bitwise ORs a register and an immediate value."
    0x56 ORI(RegisterId, RegisterId, Immediate12),
    "Left shifts a register by an immediate value."
    0x57 SLLI(RegisterId, RegisterId, Immediate12),
    "Right shifts a register by an immediate value."
    0x58 SRLI(RegisterId, RegisterId, Immediate12),
    "Subtracts a register and an immediate value."
    0x59 SUBI(RegisterId, RegisterId, Immediate12),
    "Bitwise XORs a register and an immediate value."
    0x5A XORI(RegisterId, RegisterId, Immediate12),
    "Conditional jump."
    0x5B JNEI(RegisterId, RegisterId, Immediate12),
    "A byte is loaded from the specified address offset by an immediate value."
    0x5C LB(RegisterId, RegisterId, Immediate12),
    "A word is loaded from the specified address offset by an immediate value."
    0x5D LW(RegisterId, RegisterId, Immediate12),
    "Write the least significant byte of a register to memory."
    0x5E SB(RegisterId, RegisterId, Immediate12),
    "Write a register to memory."
    0x5F SW(RegisterId, RegisterId, Immediate12),
    "Copy an immediate number of bytes in memory."
    0x60 MCPI(RegisterId, RegisterId, Immediate12),
    "Get transaction fields."
    0x61 GTF(RegisterId, RegisterId, Immediate12),

    "Clear an immediate number of bytes in memory."
    0x70 MCLI(RegisterId, Immediate18),
    "Get metadata from memory."
    0x71 GM(RegisterId, Immediate18),
    "Copy immediate value into a register"
    0x72 MOVI(RegisterId, Immediate18),
    "Conditional jump against zero."
    0x73 JNZI(RegisterId, Immediate18),

    "Jump."
    0x90 JI(Immediate24),
    "Extend the current call frame's stack by an immediate value."
    0x91 CFEI(Immediate24),
    "Shrink the current call frame's stack by an immediate value."
    0x92 CFSI(Immediate24),
}
