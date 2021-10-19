//! FuelVM opcodes representation

#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

mod opcode;
mod parsed_opcode;

pub use fuel_types::{Immediate06, Immediate12, Immediate18, Immediate24, RegisterId, Word};
pub use opcode::Opcode;
pub use parsed_opcode::ParsedOpcode;
