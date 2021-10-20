//! FuelVM opcodes representation

#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

mod instruction_fields;
mod opcode;

pub use fuel_types::{Immediate06, Immediate12, Immediate18, Immediate24, RegisterId, Word};
pub use instruction_fields::InstructionFields;
pub use opcode::Opcode;
