use fuel_types::Word;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "serde-types-minimal",
    derive(serde::Serialize, serde::Deserialize)
)]
/// Panic reason representation for the interpreter.
pub enum PanicReason {
    /// Successful execution.
    Success = 0x00,
    /// Found `RVRT` instruction.
    Revert = 0x01,
    /// Execution ran out of gas.
    OutOfGas = 0x02,
    /// The transaction validity is violated.
    TransactionValidity = 0x03,
    /// Attempt to write outside interpreter memory boundaries.
    MemoryOverflow = 0x04,
    /// Overflow while executing arithmetic operation.
    ArithmeticOverflow = 0x05,
    /// Designed contract was not found in the storage.
    ContractNotFound = 0x06,
    /// Memory ownership rules are violated.
    MemoryOwnership = 0x07,
    /// The color balance isn't enough for the instruction.
    NotEnoughBalance = 0x08,
    /// The interpreter is expected to be in internal context.
    ExpectedInternalContext = 0x09,
    /// The queried color was not found in the state.
    ColorNotFound = 0x0a,
    /// The provided input is not found in the transaction.
    InputNotFound = 0x0b,
    /// The provided output is not found in the transaction.
    OutputNotFound = 0x0c,
    /// The provided witness is not found in the transaction.
    WitnessNotFound = 0x0d,
    /// The transaction maturity is not valid for this request.
    TransactionMaturity = 0x0e,
    /// The metadata identifier is invalid.
    InvalidMetadataIdentifier = 0x0f,
    /// The call structure is not valid.
    MalformedCallStructure = 0x10,
    /// The provided register does not allow write operations.
    ReservedRegisterNotWritable = 0x11,
    /// The execution resulted in an erroneous state of the interpreter.
    ErrorFlag = 0x12,
    /// The provided immediate value is not valid for this instruction.
    InvalidImmediateValue = 0x13,
    /// The provided transaction input is not of type `Coin`.
    ExpectedCoinInput = 0x14,
    /// The requested memory access exceeds the limits of the interpreter.
    MaxMemoryAccess = 0x15,
    /// Two segments of the interpreter memory should not intersect for write operations.
    MemoryWriteOverlap = 0x16,
    /// The requested contract is not listed in the transaction inputs.
    ContractNotInInputs = 0x17,
    /// The internal color balance overflowed with the provided instruction.
    InternalBalanceOverflow = 0x18,
    /// The maximum allowed contract size is violated.
    ContractMaxSize = 0x19,
    /// This instruction expects the stack area to be unallocated for this call.
    ExpectedUnallocatedStack = 0x1a,
    /// The maximum allowed number of static contracts was reached for this transaction.
    MaxStaticContractsReached = 0x1b,
    /// The requested transfer amount cannot be zero.
    TransferAmountCannotBeZero = 0x1c,
    /// The provided transaction output should be of type `Variable`.
    ExpectedOutputVariable = 0x1d,
    /// The expected context of the stack parent is internal.
    ExpectedParentInternalContext = 0x1e,
    /// This panic representation is not valid.
    InvalidRepresentation = 0xff,
}

impl From<PanicReason> for Word {
    fn from(r: PanicReason) -> Word {
        r as Word
    }
}

impl From<Word> for PanicReason {
    fn from(b: Word) -> Self {
        match b {
            0x00 => Self::Success,
            0x01 => Self::Revert,
            0x02 => Self::OutOfGas,
            0x03 => Self::TransactionValidity,
            0x04 => Self::MemoryOverflow,
            0x05 => Self::ArithmeticOverflow,
            0x06 => Self::ContractNotFound,
            0x07 => Self::MemoryOwnership,
            0x08 => Self::NotEnoughBalance,
            0x09 => Self::ExpectedInternalContext,
            0x0a => Self::ColorNotFound,
            0x0b => Self::InputNotFound,
            0x0c => Self::OutputNotFound,
            0x0d => Self::WitnessNotFound,
            0x0e => Self::TransactionMaturity,
            0x0f => Self::InvalidMetadataIdentifier,
            0x10 => Self::MalformedCallStructure,
            0x11 => Self::ReservedRegisterNotWritable,
            0x12 => Self::ErrorFlag,
            0x13 => Self::InvalidImmediateValue,
            0x14 => Self::ExpectedCoinInput,
            0x15 => Self::MaxMemoryAccess,
            0x16 => Self::MemoryWriteOverlap,
            0x17 => Self::ContractNotInInputs,
            0x18 => Self::InternalBalanceOverflow,
            0x19 => Self::ContractMaxSize,
            0x1a => Self::ExpectedUnallocatedStack,
            0x1b => Self::MaxStaticContractsReached,
            0x1c => Self::TransferAmountCannotBeZero,
            0x1d => Self::ExpectedOutputVariable,
            0x1e => Self::ExpectedParentInternalContext,
            _ => Self::InvalidRepresentation,
        }
    }
}
