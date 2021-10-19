use fuel_types::{Immediate06, Immediate12, Immediate18, Immediate24, RegisterId, Word};

use core::convert::TryFrom;

#[cfg(feature = "std")]
use std::io;

use crate::opcode::consts::OpcodeRepr;

/// A version of Opcode that can used without unnecessary branching
#[derive(Debug, Clone, Copy)]
pub struct ParsedOpcode {
    pub(super) op: u8,
    pub(super) ra: RegisterId,
    pub(super) rb: RegisterId,
    pub(super) rc: RegisterId,
    pub(super) rd: RegisterId,
    pub(super) _imm06: Immediate06,
    pub(super) imm12: Immediate12,
    pub(super) imm18: Immediate18,
    pub(super) imm24: Immediate24,
}

impl ParsedOpcode {
    /// Size of the struct when serialized into bytes
    pub const BYTES_SIZE: usize = 4;

    /// Create a `Opcode` from a slice of bytes
    ///
    /// # Panics
    ///
    /// This function will panic if the length of the bytes is smaller than
    /// [`Opcode::BYTES_SIZE`].
    pub fn from_bytes_unchecked(bytes: &[u8]) -> Self {
        assert!(Self::BYTES_SIZE <= bytes.len());

        <[u8; Self::BYTES_SIZE]>::try_from(&bytes[..Self::BYTES_SIZE])
            .map(u32::from_be_bytes)
            .map(Self::from)
            .unwrap_or_else(|_| unreachable!())
    }

    /// Convert the opcode to bytes representation
    pub fn to_bytes(self) -> [u8; Self::BYTES_SIZE] {
        u32::from(self).to_be_bytes()
    }

    /// Splits a Word into two [`ParsedOpcode`] that can be used to construct [`Opcode`]
    pub fn parse_word(word: Word) -> (ParsedOpcode, ParsedOpcode) {
        // Assumes Word is u64
        let lo = word as u32;
        let hi = (word >> 32) as u32;

        (ParsedOpcode::from(lo), ParsedOpcode::from(hi))
    }
}

impl From<u32> for ParsedOpcode {
    fn from(instruction: u32) -> Self {
        // TODO Optimize with native architecture (eg SIMD?) or facilitate
        // auto-vectorization

        let op = (instruction >> 24) as u8;

        let ra = ((instruction >> 18) & 0x3f) as RegisterId;
        let rb = ((instruction >> 12) & 0x3f) as RegisterId;
        let rc = ((instruction >> 6) & 0x3f) as RegisterId;
        let rd = (instruction & 0x3f) as RegisterId;

        let _imm06 = (instruction & 0xff) as Immediate06;
        let imm12 = (instruction & 0x0fff) as Immediate12;
        let imm18 = (instruction & 0x3ffff) as Immediate18;
        let imm24 = (instruction & 0xffffff) as Immediate24;

        Self {
            op,
            ra,
            rb,
            rc,
            rd,
            _imm06,
            imm12,
            imm18,
            imm24,
        }
    }
}

impl From<ParsedOpcode> for u32 {
    fn from(parsed: ParsedOpcode) -> u32 {
        let a = (parsed.ra as u32) << 18;
        let b = (parsed.rb as u32) << 12;
        let c = (parsed.rc as u32) << 6;
        let d = parsed.rd as u32;

        let imm12 = parsed.imm12 as u32;
        let imm18 = parsed.imm18 as u32;
        let imm24 = parsed.imm24 as u32;

        let args = match OpcodeRepr::from_u8(parsed.op) {
            OpcodeRepr::ADD => a | b | c,
            OpcodeRepr::ADDI => a | b | imm12,
            OpcodeRepr::AND => a | b | c,
            OpcodeRepr::ANDI => a | b | imm12,
            OpcodeRepr::DIV => a | b | c,
            OpcodeRepr::DIVI => a | b | imm12,
            OpcodeRepr::EQ => a | b | c,
            OpcodeRepr::EXP => a | b | c,
            OpcodeRepr::EXPI => a | b | imm12,
            OpcodeRepr::GT => a | b | c,
            OpcodeRepr::LT => a | b | c,
            OpcodeRepr::MLOG => a | b | c,
            OpcodeRepr::MROO => a | b | c,
            OpcodeRepr::MOD => a | b | c,
            OpcodeRepr::MODI => a | b | imm12,
            OpcodeRepr::MOVE => a | b,
            OpcodeRepr::MUL => a | b | c,
            OpcodeRepr::MULI => a | b | imm12,
            OpcodeRepr::NOT => a | b,
            OpcodeRepr::OR => a | b | c,
            OpcodeRepr::ORI => a | b | imm12,
            OpcodeRepr::SLL => a | b | c,
            OpcodeRepr::SLLI => a | b | imm12,
            OpcodeRepr::SRL => a | b | c,
            OpcodeRepr::SRLI => a | b | imm12,
            OpcodeRepr::SUB => a | b | c,
            OpcodeRepr::SUBI => a | b | imm12,
            OpcodeRepr::XOR => a | b | c,
            OpcodeRepr::XORI => a | b | imm12,
            OpcodeRepr::CIMV => a | b | c,
            OpcodeRepr::CTMV => a | b,
            OpcodeRepr::JI => imm24,
            OpcodeRepr::JNEI => a | b | imm12,
            OpcodeRepr::RET => a,
            OpcodeRepr::RETD => a | b,
            OpcodeRepr::CFEI => imm24,
            OpcodeRepr::CFSI => imm24,
            OpcodeRepr::LB => a | b | imm12,
            OpcodeRepr::LW => a | b | imm12,
            OpcodeRepr::ALOC => a,
            OpcodeRepr::MCL => a | b,
            OpcodeRepr::MCLI => a | imm18,
            OpcodeRepr::MCP => a | b | c,
            OpcodeRepr::MEQ => a | b | c | d,
            OpcodeRepr::SB => a | b | imm12,
            OpcodeRepr::SW => a | b | imm12,
            OpcodeRepr::BHSH => a | b,
            OpcodeRepr::BHEI => a,
            OpcodeRepr::BURN => a,
            OpcodeRepr::CALL => a | b | c | d,
            OpcodeRepr::CCP => a | b | c | d,
            OpcodeRepr::CROO => a | b,
            OpcodeRepr::CSIZ => a | b,
            OpcodeRepr::CB => a,
            OpcodeRepr::LDC => a | b | c,
            OpcodeRepr::LOG => a | b | c | d,
            OpcodeRepr::LOGD => a | b | c | d,
            OpcodeRepr::MINT => a,
            OpcodeRepr::RVRT => a,
            OpcodeRepr::SLDC => a | b | c,
            OpcodeRepr::SRW => a | b,
            OpcodeRepr::SRWQ => a | b,
            OpcodeRepr::SWW => a | b,
            OpcodeRepr::SWWQ => a | b,
            OpcodeRepr::TR => a | b | c,
            OpcodeRepr::TRO => a | b | c | d,
            OpcodeRepr::ECR => a | b | c,
            OpcodeRepr::K256 => a | b | c,
            OpcodeRepr::S256 => a | b | c,
            OpcodeRepr::XIL => a | b,
            OpcodeRepr::XIS => a | b,
            OpcodeRepr::XOL => a | b,
            OpcodeRepr::XOS => a | b,
            OpcodeRepr::XWL => a | b,
            OpcodeRepr::XWS => a | b,
            OpcodeRepr::NOOP => 0,
            OpcodeRepr::FLAG => a,
            OpcodeRepr::GM => a | imm18,
            OpcodeRepr::UNDEFINED => 0,
        };

        ((parsed.op as u32) << 24) | args
    }
}

#[cfg(feature = "std")]
impl ParsedOpcode {
    /// Create a `ParsedOpcode` from a slice of bytes
    ///
    /// This function will fail if the length of the bytes is smaller than
    /// [`ParsedOpcode::BYTES_SIZE`].
    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        if bytes.len() < Self::BYTES_SIZE {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "The provided buffer is not big enough!",
            ))
        } else {
            Ok(Self::from_bytes_unchecked(bytes))
        }
    }
}
