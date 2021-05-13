pub mod main;
pub mod chunk;

pub use chunk::Chunk;
pub use main::{BytecodeCompiler, Opcode};
pub use dashscript_ast::fsize;