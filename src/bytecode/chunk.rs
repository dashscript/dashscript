use dashscript_ast::{ConstantPool, Position};
use crate::BytecodeCompiler;

#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub bytes: Vec<u8>,
    pub constants: ConstantPool,
    pub position_map: Vec<(usize, Position)>,
    pub chunk_map: Vec<Vec<u8>>
}

impl From<BytecodeCompiler> for Chunk {
    fn from(compiler: BytecodeCompiler) -> Self {
        Self {
            bytes: compiler.bytes,
            constants: compiler.ast.constant_pool,
            position_map: compiler.pos_map,
            chunk_map: compiler.chunk_map
        }
    }
}