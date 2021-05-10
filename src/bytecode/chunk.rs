use dashscript_ast::{ConstantPool, Position};

pub struct Chunk {
    pub bytes: Vec<u8>,
    pub constants: ConstantPool,
    pub position_map: Vec<(usize, Position)>
}