use fuel_types::{Immediate06, Immediate12, Immediate18, Immediate24, RegisterId, Word};

#[cfg(feature = "std")]
use std::io;

use crate::opcode::consts::OpcodeRepr;

/// A version of Opcode that can used without unnecessary branching
#[derive(Debug, Clone, Copy)]
pub struct InstructionFields {
    /// Opcode
    pub op: u8,
    /// Register A
    pub ra: RegisterId,
    /// Register B
    pub rb: RegisterId,
    /// Register C
    pub rc: RegisterId,
    /// Register D
    pub rd: RegisterId,
    /// Immediate with 6 bits
    pub imm06: Immediate06,
    /// Immediate with 12 bits
    pub imm12: Immediate12,
    /// Immediate with 18 bits
    pub imm18: Immediate18,
    /// Immediate with 24 bits
    pub imm24: Immediate24,
}

impl InstructionFields {
    /// Size of an opcode in bytes
    pub const LEN: usize = 4;

    /// Extracts fields from a raw instruction
    pub const fn new(instruction: u32) -> Self {
        // TODO Optimize with native architecture (eg SIMD?) or facilitate
        // auto-vectorization

        let op = (instruction >> 24) as u8;

        let ra = ((instruction >> 18) & 0x3f) as RegisterId;
        let rb = ((instruction >> 12) & 0x3f) as RegisterId;
        let rc = ((instruction >> 6) & 0x3f) as RegisterId;
        let rd = (instruction & 0x3f) as RegisterId;

        let imm06 = (instruction & 0xff) as Immediate06;
        let imm12 = (instruction & 0x0fff) as Immediate12;
        let imm18 = (instruction & 0x3ffff) as Immediate18;
        let imm24 = (instruction & 0xffffff) as Immediate24;

        Self {
            op,
            ra,
            rb,
            rc,
            rd,
            imm06,
            imm12,
            imm18,
            imm24,
        }
    }

    /// Create a `InstructionFields` from a slice of bytes
    ///
    /// # Safety
    ///
    /// The caller must ensure that the slice is has at least `Self::LEN` bytes.
    pub unsafe fn from_slice_unchecked(buf: &[u8]) -> Self {
        debug_assert!(buf.len() >= 4);
        let arr: [u8; Self::LEN] = fuel_types::bytes::from_slice_unchecked(buf);
        Self::from(u32::from_be_bytes(arr))
    }

    /// Convert the opcode to bytes representation
    pub fn to_bytes(self) -> [u8; Self::LEN] {
        u32::from(self).to_be_bytes()
    }

    /// Splits a Word into two [`InstructionFields`] that can be used to construct [`Opcode`]
    pub const fn parse_word(word: Word) -> (InstructionFields, InstructionFields) {
        // Assumes Word is u64
        let lo = word as u32;
        let hi = (word >> 32) as u32;

        (InstructionFields::new(hi), InstructionFields::new(lo))
    }
}

impl From<u32> for InstructionFields {
    fn from(instruction: u32) -> Self {
        Self::new(instruction)
    }
}

impl From<InstructionFields> for u32 {
    fn from(parsed: InstructionFields) -> u32 {
        let a = (parsed.ra as u32) << 18;
        let b = (parsed.rb as u32) << 12;
        let c = (parsed.rc as u32) << 6;
        let d = parsed.rd as u32;

        let imm12 = parsed.imm12 as u32;
        let imm18 = parsed.imm18 as u32;
        let imm24 = parsed.imm24 as u32;

        let args = match OpcodeRepr::from_u8(parsed.op) {
            OpcodeRepr::ADD
            | OpcodeRepr::AND
            | OpcodeRepr::DIV
            | OpcodeRepr::EQ
            | OpcodeRepr::EXP
            | OpcodeRepr::GT
            | OpcodeRepr::LT
            | OpcodeRepr::MLOG
            | OpcodeRepr::MROO
            | OpcodeRepr::MOD
            | OpcodeRepr::MUL
            | OpcodeRepr::OR
            | OpcodeRepr::SLL
            | OpcodeRepr::SRL
            | OpcodeRepr::SUB
            | OpcodeRepr::XOR
            | OpcodeRepr::CIMV
            | OpcodeRepr::MCP
            | OpcodeRepr::LDC
            | OpcodeRepr::SLDC
            | OpcodeRepr::TR
            | OpcodeRepr::ECR
            | OpcodeRepr::K256
            | OpcodeRepr::S256 => a | b | c,
            OpcodeRepr::ADDI
            | OpcodeRepr::ANDI
            | OpcodeRepr::DIVI
            | OpcodeRepr::EXPI
            | OpcodeRepr::MODI
            | OpcodeRepr::MULI
            | OpcodeRepr::ORI
            | OpcodeRepr::SLLI
            | OpcodeRepr::SRLI
            | OpcodeRepr::SUBI
            | OpcodeRepr::XORI
            | OpcodeRepr::JNEI
            | OpcodeRepr::LB
            | OpcodeRepr::LW
            | OpcodeRepr::SB
            | OpcodeRepr::SW => a | b | imm12,
            OpcodeRepr::MOVE
            | OpcodeRepr::NOT
            | OpcodeRepr::CTMV
            | OpcodeRepr::RETD
            | OpcodeRepr::MCL
            | OpcodeRepr::BHSH
            | OpcodeRepr::CROO
            | OpcodeRepr::CSIZ
            | OpcodeRepr::SRW
            | OpcodeRepr::SRWQ
            | OpcodeRepr::SWW
            | OpcodeRepr::SWWQ
            | OpcodeRepr::XIL
            | OpcodeRepr::XIS
            | OpcodeRepr::XOL
            | OpcodeRepr::XOS
            | OpcodeRepr::XWL
            | OpcodeRepr::XWS => a | b,
            OpcodeRepr::RET
            | OpcodeRepr::ALOC
            | OpcodeRepr::BHEI
            | OpcodeRepr::BURN
            | OpcodeRepr::CB
            | OpcodeRepr::MINT
            | OpcodeRepr::RVRT
            | OpcodeRepr::FLAG => a,
            OpcodeRepr::JI | OpcodeRepr::CFEI | OpcodeRepr::CFSI => imm24,
            OpcodeRepr::MCLI | OpcodeRepr::GM => a | imm18,
            OpcodeRepr::MEQ
            | OpcodeRepr::CALL
            | OpcodeRepr::CCP
            | OpcodeRepr::LOG
            | OpcodeRepr::LOGD
            | OpcodeRepr::TRO => a | b | c | d,
            OpcodeRepr::NOOP | OpcodeRepr::UNDEFINED => 0,
        };

        ((parsed.op as u32) << 24) | args
    }
}

#[cfg(feature = "std")]
impl InstructionFields {
    /// Create a `InstructionFields` from a slice of bytes
    ///
    /// This function will fail if the length of the bytes is smaller than
    /// [`InstructionFields::LEN`].
    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        if bytes.len() < Self::LEN {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "The provided buffer is not big enough!",
            ))
        } else {
            // Safety: we check the length above
            unsafe { Ok(Self::from_slice_unchecked(bytes)) }
        }
    }
}
