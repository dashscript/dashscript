use crate::{ConstantPool, Position, BytecodeCompiler};

#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub(crate) bytes: Vec<u8>,
    pub(crate) constants: ConstantPool,
    pub(crate) try_blocks: Vec<(usize, usize, u8)>,
    pub(crate) position_map: Vec<(usize, Position)>,
    pub(crate) line_data: Vec<u32>
}

impl Chunk {

    pub fn get_position(&self, ip: usize) -> Position {
        for (index, position) in self.position_map.iter() {
            if ip <= *index {
                return *position;
            }
        }

        Position::default()
    }

    pub fn get_line(&self, position: Position) -> u32 {
        let mut current_index = 0;
        let mut index = 0;

        for line_length in self.line_data.iter() {
            current_index += line_length;
            index += 1;

            if current_index >= position.start as u32 {
                return index;
            }
        }

        index
    }
    
}

impl From<BytecodeCompiler> for Chunk {
    fn from(mut compiler: BytecodeCompiler) -> Self {
        Self {
            bytes: compiler.bytes,
            constants: compiler.ast.constant_pool,
            position_map: compiler.position_map,
            line_data: compiler.line_data,
            try_blocks: {
                compiler.try_blocks.reverse();
                compiler.try_blocks
            }
        }
    }
}