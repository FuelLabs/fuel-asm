/// Generates the following:
///
/// - A unique type for each opcode instruction type.
/// - Register and immediate value access methods for each opcode instruction type.
/// - An enum over all possible opcodes.
/// - An enum over all possible instructions.
macro_rules! impl_opcodes {
    // Recursively declares a unique struct for each opcode.
    (decl_op_struct $doc:literal $ix:literal $Op:ident [$($field:ident)*] $($rest:tt)*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[doc = $doc]
        pub struct $Op([u8; 3]);
        impl_opcodes!(decl_op_struct $($rest)*);
    };
    (decl_op_struct) => {};

    // Define the `OpcodeRepr` enum.
    (decl_opcode_enum $($doc:literal $ix:literal $Op:ident [$($field:ident)*])*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[repr(u8)]
        pub enum Opcode {
            $(
                #[doc = $doc]
                $Op = $ix,
            )*
        }
    };

    // Define the `Opcode` enum.
    (decl_instruction_enum $($doc:literal $ix:literal $Op:ident [$($field:ident)*])*) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum Instruction {
            $(
                #[doc = $doc]
                $Op($Op),
            )*
        }
    };

    // // Select a `u32_from_op_bytes` conversion based on the instruction layout.
    // (u32_from_op_bytes [] $opcode:literal $bytes:expr) => {{
    //     u32_from_op($opcode)
    // }};
    // (u32_from_op_bytes [reg] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [reg reg] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_rb_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [reg reg reg] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_rb_rc_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [reg reg reg reg] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_rb_rc_rd_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [reg reg imm12] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_rb_imm12_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [reg imm18] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_ra_imm18_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [imm24] $opcode:literal $bytes:expr) => {{
    //     u32_from_op_imm24_bytes($opcode, $bytes)
    // }};
    // (u32_from_op_bytes [$($fields:ident)*] $opcode:literal $bytes:expr) => {
    //     compile_error!("unhandled `Instruction` layout for u32 conversion")
    // };

    (impl_op_new []) => {
        pub fn new() -> Self {
            Self([0; 3])
        }
    };
    (impl_op_new [RegId]) => {
        pub fn new(ra: u8) -> Self {
            Self(bytes_from_ra(ra))
        }
    };

    // Implement constructors and accessors for register and immediate values.
    (impl_op $doc:literal $ix:literal $Op:ident [$($field:ident)*] $($rest:tt)*) => {
        impl $Op {
            /// The associated 8-bit Opcode value.
            pub const OPCODE: Opcode = Opcode::$Op;

            //impl_opcodes!(impl_op_new);

            //impl_opcodes!(impl_op_accessors);
        }

        impl From<$Op> for [u8; 3] {
            fn from($Op(arr): $Op) -> Self {
                arr
            }
        }

        impl From<$Op> for [u8; 4] {
            fn from($Op([a, b, c]): $Op) -> Self {
                [$Op::OPCODE as u8, a, b, c]
            }
        }

        impl From<$Op> for u32 {
            #[allow(unused)]
            fn from($Op([a, b, c]): $Op) -> Self {
                u32::from_be_bytes([$Op::OPCODE as u8, a, b, c])
            }
        }

        impl_opcodes!(impl_op $($rest)*);
    };
    (impl_op) => {};

    // Implement `TryFrom<u8>` for `Opcode`.
    (impl_opcode $($doc:literal $ix:literal $Op:ident [$($field:ident)*])*) => {
        impl core::convert::TryFrom<u8> for Opcode {
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

        impl Into<u8> for Opcode {
            fn into(self) -> u8 {
                self as u8
            }
        }

        #[test]
        fn test_opcode_u8_conv() {
            for u in 0..=u8::MAX {
                if let Ok(op) = Opcode::try_from(u) {
                    assert_eq!(op as u8, u);
                }
            }
        }
    };

    // Implement accessors for register and immediate values.
    (impl_instruction $($doc:literal $ix:literal $Op:ident [$($field:ident)*])*) => {
        impl Instruction {
            /// This instruction's opcode.
            pub fn opcode(&self) -> Opcode {
                match self {
                    $(
                        Self::$Op(_) => Opcode::$Op,
                    )*
                }
            }
        }

        // impl From<Instruction> for u32 {
        //     fn from(inst: Instruction) -> Self {
        //         match inst {
        //             $(
        //                 Self::$Op($Op([a, b, c])) => u32::from_be_bytes([Opcode::$Op as u8, a, b, c]),
        //             )*
        //         }
        //     }
        // }

        // impl core::convert::TryFrom<[u8; 4]> for Instruction {
        //     type Error = InvalidOpcode;
        //     fn try_from([op, a, b, c]: [u8; 4]) -> Result<Self, Self::Error> {
        //         match Opcode::try_from(op)? {
        //             $(
        //                 Opcode::$Op => Ok(Self::$Op($Op([a, b, c]))),
        //             )*
        //         }
        //     }
        // }

        // impl core::convert::TryFrom<u32> for Instruction {
        //     type Error = InvalidOpcode;
        //     fn try_from(u: u32) -> Result<Self, Self::Error> {
        //         Self::try_from(u.to_be_bytes())
        //     }
        // }
    };

    // Entrypoint to the macro, generates struct, methods, enum and instruction enum separately.
    ($($tts:tt)*) => {
        impl_opcodes!(decl_op_struct $($tts)*);
        impl_opcodes!(decl_opcode_enum $($tts)*);
        impl_opcodes!(decl_instruction_enum $($tts)*);
        impl_opcodes!(impl_op $($tts)*);
        impl_opcodes!(impl_opcode $($tts)*);
        impl_opcodes!(impl_instruction $($tts)*);
    };
}

/// Represents a 6-bit register ID, guaranteed to be masked by construction.
pub struct RegId(u8);

/// Represents a 12-bit immediate value, guaranteed to be masked by construction.
pub struct Imm12(u16);

/// Represents a 18-bit immediate value, guaranteed to be masked by construction.
pub struct Imm18(u32);

/// Represents a 24-bit immediate value, guaranteed to be masked by construction.
pub struct Imm24(u32);

/// Failed to parse a `u8` as a valid or non-reserved opcode.
#[derive(Debug)]
pub struct InvalidOpcode;

impl RegId {
    /// Construct a register ID from the given value.
    ///
    /// The given value will be masked to 6 bits.
    pub fn new(u: u8) -> Self {
        Self(u & 0b_00111111)
    }
}

impl Imm12 {
    /// Construct an immediate value.
    ///
    /// The given value will be masked to 12 bits.
    pub fn new(u: u16) -> Self {
        Self(u & 0b_00001111_11111111)
    }
}

impl Imm18 {
    /// Construct an immediate value.
    ///
    /// The given value will be masked to 18 bits.
    pub fn new(u: u32) -> Self {
        Self(u & 0b_00000000_00000011_11111111_11111111)
    }
}

impl Imm24 {
    /// Construct an immediate value.
    ///
    /// The given value will be masked to 24 bits.
    pub fn new(u: u32) -> Self {
        Self(u & 0b_00000000_11111111_11111111_11111111)
    }
}

impl From<RegId> for u8 {
    fn from(RegId(u): RegId) -> Self {
        u
    }
}

impl From<Imm12> for u16 {
    fn from(Imm12(u): Imm12) -> Self {
        u
    }
}

impl From<Imm18> for u32 {
    fn from(Imm18(u): Imm18) -> Self {
        u
    }
}

impl From<Imm24> for u32 {
    fn from(Imm24(u): Imm24) -> Self {
        u
    }
}

impl_opcodes! {
    "Adds two registers."
    0x10 ADD [RegId RegId RegId]
    "Bitwise ANDs two registers."
    0x11 AND [RegId RegId RegId]
    "Divides two registers."
    0x12 DIV [RegId RegId RegId]
    "Compares two registers for equality."
    0x13 EQ [RegId RegId RegId]
    "Raises one register to the power of another."
    0x14 EXP [RegId RegId RegId]
    "Compares two registers for greater-than."
    0x15 GT [RegId RegId RegId]
    "Compares two registers for less-than."
    0x16 LT [RegId RegId RegId]
    "The integer logarithm of a register."
    0x17 MLOG [RegId RegId RegId]
    "The integer root of a register."
    0x18 MROO [RegId RegId RegId]
    "Modulo remainder of two registers."
    0x19 MOD [RegId RegId RegId]
    "Copy from one register to another."
    0x1A MOVE [RegId RegId]
    "Multiplies two registers."
    0x1B MUL [RegId RegId RegId]
    "Bitwise NOT a register."
    0x1C NOT [RegId RegId]
    "Bitwise ORs two registers."
    0x1D OR [RegId RegId RegId]
    "Left shifts a register by a register."
    0x1E SLL [RegId RegId RegId]
    "Right shifts a register by a register."
    0x1F SRL [RegId RegId RegId]
    "Subtracts two registers."
    0x20 SUB [RegId RegId RegId]
    "Bitwise XORs two registers."
    0x21 XOR [RegId RegId RegId]

    "Return from context."
    0x24 RET [RegId]
    "Return from context with data."
    0x25 RETD [RegId RegId]
    "Allocate a number of bytes from the heap."
    0x26 ALOC [RegId]
    "Clear a variable number of bytes in memory."
    0x27 MCL [RegId RegId]
    "Copy a variable number of bytes in memory."
    0x28 MCP [RegId RegId RegId]
    "Compare bytes in memory."
    0x29 MEQ [RegId RegId RegId RegId]
    "Get block header hash for height."
    0x2A BHSH [RegId RegId]
    "Get current block height."
    0x2B BHEI [RegId]
    "Burn coins of the current contract's asset ID."
    0x2C BURN [RegId]
    "Call a contract."
    0x2D CALL [RegId RegId RegId RegId]
    "Copy contract code for a contract."
    0x2E CCP [RegId RegId RegId RegId]
    "Get code root of a contract."
    0x2F CROO [RegId RegId]
    "Get code size of a contract."
    0x30 CSIZ [RegId RegId]
    "Get current block proposer's address."
    0x31 CB [RegId]
    "Load a contract's code as executable."
    0x32 LDC [RegId RegId RegId]
    "Log an event."
    0x33 LOG [RegId RegId RegId RegId]
    "Log data."
    0x34 LOGD [RegId RegId RegId RegId]
    "Mint coins of the current contract's asset ID."
    0x35 MINT [RegId]
    "Halt execution, reverting state changes and returning a value."
    0x36 RVRT [RegId]
    "Clear a series of slots from contract storage."
    0x37 SCWQ [RegId RegId RegId]
    "Load a word from contract storage."
    0x38 SRW [RegId RegId RegId]
    "Load a series of 32 byte slots from contract storage."
    0x39 SRWQ [RegId RegId RegId RegId]
    "Store a word in contract storage."
    0x3A SWW [RegId RegId RegId]
    "Store a series of 32 byte slots in contract storage."
    0x3B SWWQ [RegId RegId RegId RegId]
    "Transfer coins to a contract unconditionally."
    0x3C TR [RegId RegId RegId]
    "Transfer coins to a variable output."
    0x3D TRO [RegId RegId RegId RegId]
    "The 64-byte public key (x, y) recovered from 64-byte signature on 32-byte message."
    0x3E ECR [RegId RegId RegId]
    "The keccak-256 hash of a slice."
    0x3F K256 [RegId RegId RegId]
    "The SHA-2-256 hash of a slice."
    0x40 S256 [RegId RegId RegId]
    "Get timestamp of block at given height."
    0x41 TIME [RegId RegId]

    "Performs no operation."
    0x47 NOOP []
    "Set flag register to a register."
    0x48 FLAG [RegId]
    "Get the balance of contract of an asset ID."
    0x49 BAL [RegId RegId RegId]
    "Dynamic jump."
    0x4A JMP [RegId]
    "Conditional dynamic jump."
    0x4B JNE [RegId RegId RegId]
    "Send a message to recipient address with call abi, coins, and output."
    0x4C SMO [RegId RegId RegId RegId]

    "Adds a register and an immediate value."
    0x50 ADDI [RegId RegId Imm12]
    "Bitwise ANDs a register and an immediate value."
    0x51 ANDI [RegId RegId Imm12]
    "Divides a register and an immediate value."
    0x52 DIVI [RegId RegId Imm12]
    "Raises one register to the power of an immediate value."
    0x53 EXPI [RegId RegId Imm12]
    "Modulo remainder of a register and an immediate value."
    0x54 MODI [RegId RegId Imm12]
    "Multiplies a register and an immediate value."
    0x55 MULI [RegId RegId Imm12]
    "Bitwise ORs a register and an immediate value."
    0x56 ORI [RegId RegId Imm12]
    "Left shifts a register by an immediate value."
    0x57 SLLI [RegId RegId Imm12]
    "Right shifts a register by an immediate value."
    0x58 SRLI [RegId RegId Imm12]
    "Subtracts a register and an immediate value."
    0x59 SUBI [RegId RegId Imm12]
    "Bitwise XORs a register and an immediate value."
    0x5A XORI [RegId RegId Imm12]
    "Conditional jump."
    0x5B JNEI [RegId RegId Imm12]
    "A byte is loaded from the specified address offset by an immediate value."
    0x5C LB [RegId RegId Imm12]
    "A word is loaded from the specified address offset by an immediate value."
    0x5D LW [RegId RegId Imm12]
    "Write the least significant byte of a register to memory."
    0x5E SB [RegId RegId Imm12]
    "Write a register to memory."
    0x5F SW [RegId RegId Imm12]
    "Copy an immediate number of bytes in memory."
    0x60 MCPI [RegId RegId Imm12]
    "Get transaction fields."
    0x61 GTF [RegId RegId Imm12]

    "Clear an immediate number of bytes in memory."
    0x70 MCLI [RegId Imm18]
    "Get metadata from memory."
    0x71 GM [RegId Imm18]
    "Copy immediate value into a register"
    0x72 MOVI [RegId Imm18]
    "Conditional jump against zero."
    0x73 JNZI [RegId Imm18]

    "Jump."
    0x90 JI [Imm24]
    "Extend the current call frame's stack by an immediate value."
    0x91 CFEI [Imm24]
    "Shrink the current call frame's stack by an immediate value."
    0x92 CFSI [Imm24]
}

// fn u32_from_op(op: u8) -> u32 {
//     (op as u32) << 24
// }
//
// fn u32_from_op_ra(op: u8, ra: u8) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18
// }
//
// fn u32_from_op_ra_rb(op: u8, ra: u8, rb: u8) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18 | (rb as u32) << 12
// }
//
// fn u32_from_op_ra_rb_rc(op: u8, ra: u8, rb: u8, rc: u8) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18 | (rb as u32) << 12 | (rc as u32) << 6
// }
//
// fn u32_from_op_ra_rb_rc_rd(op: u8, ra: u8, rb: u8, rc: u8, rd: u8) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18 | (rb as u32) << 12 | (rc as u32) << 6 | rd as u32
// }
//
// fn u32_from_op_ra_rb_imm12(op: u8, ra: u8, rb: u8, imm12: u16) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18 | (rb as u32) << 12 | imm12 as u32
// }
//
// fn u32_from_op_ra_imm18(op: u8, ra: u8, imm18: u32) -> u32 {
//     (op as u32) << 24 | (ra as u32) << 18 | imm18
// }
//
// fn u32_from_op_imm24(op: u8, imm24: u32) -> u32 {
//     (op as u32) << 24 | imm24
// }
//
// fn u32_from_op_ra_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra(op, ra_from_bytes(bs))
// }
//
// fn u32_from_op_ra_rb_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra_rb(op, ra_from_bytes(bs), rb_from_bytes(bs))
// }
//
// fn u32_from_op_ra_rb_rc_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra_rb_rc(op, ra_from_bytes(bs), rb_from_bytes(bs), rc_from_bytes(bs))
// }
//
// fn u32_from_op_ra_rb_rc_rd_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra_rb_rc_rd(
//         op,
//         ra_from_bytes(bs),
//         rb_from_bytes(bs),
//         rc_from_bytes(bs),
//         rd_from_bytes(bs),
//     )
// }
//
// fn u32_from_op_ra_rb_imm12_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra_rb_imm12(op, ra_from_bytes(bs), rb_from_bytes(bs), imm12_from_bytes(bs))
// }
//
// fn u32_from_op_ra_imm18_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_ra_imm18(op, ra_from_bytes(bs), imm18_from_bytes(bs))
// }
//
// fn u32_from_op_imm24_bytes(op: u8, bs: [u8; 3]) -> u32 {
//     u32_from_op_imm24(op, imm24_from_bytes(bs))
// }

fn ra_from_bytes([a, _, _]: [u8; 3]) -> RegId {
    RegId::new(a)
}

fn rb_from_bytes([a, b, _]: [u8; 3]) -> RegId {
    RegId::new(a >> 6 | (b & 0b00001111) << 2)
}

fn rc_from_bytes([_, b, c]: [u8; 3]) -> u8 {
    RegId::new((b & 0b11110000) >> 4 | (c & 0b00000011) << 4)
}

fn rd_from_bytes([_, _, c]: [u8; 3]) -> u8 {
    c >> 2
}

fn imm12_from_bytes([_, b, c]: [u8; 3]) -> u16 {
    (b & 0b11110000) as u16 >> 4 | (c as u16) << 4
}

fn imm18_from_bytes([a, b, c]: [u8; 3]) -> u32 {
    (a & 0b11000000) as u32 >> 6
        | (b as u32) << 2
        | (c as u32) << 10
}

fn imm24_from_bytes([a, b, c]: [u8; 3]) -> u32 {
    u32::from_be_bytes([0, a, b, c])
}

// fn bytes_from_ra(ra: u8) -> [u8; 3] {
//     [ra & 0b00111111, 0, 0]
// }

// fn u32_from_ra(ra: u8) -> u32 {
//     (ra as u32) << 18
// }
//
// fn u32_from_ra_rb(ra: u8, rb: u8) -> u32 {
//     (ra as u32) << 18 | (rb as u32) << 12
// }
//
// fn u32_from_ra_rb_rc(ra: u8, rb: u8, rc: u8) -> u32 {
//     (ra as u32) << 18 | (rb as u32) << 12 | (rc as u32) << 6
// }
//
// fn u32_from_ra_rb_rc_rd(ra: u8, rb: u8, rc: u8, rd: u8) -> u32 {
//     (ra as u32) << 18 | (rb as u32) << 12 | (rc as u32) << 6 | rd as u32
// }
//
// fn u32_from_ra_rb_imm12(ra: u8, rb: u8, imm12: u16) -> u32 {
//     (ra as u32) << 18 | (rb as u32) << 12 | imm12 as u32
// }
//
// fn u32_from_ra_imm18(ra: u8, imm18: u32) -> u32 {
//     (mask_reg(ra) as u32) << 18 | imm18
// }
//
// fn u32_from_imm24(imm24: u32) -> u32 {
//     mask_imm24(imm24)
// }
