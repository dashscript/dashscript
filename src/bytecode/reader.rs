use std::convert::TryInto;
use crate::common::{ fsize, MAX_BYTES };
use crate::lexer::parser::Position;
use super::main::{ BytecodeCompiler, Opcode };

#[derive(Debug, Clone)]
pub struct BytecodeReader {
    pub bytes: Vec<u8>,
    pub constants: Vec<String>,
    pub len: usize,
    pub ci: usize,
    pub pos_map: Vec<(usize, Position)>
}

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    GreaterThan,
    LessThan
}

#[derive(Debug, Clone)]
pub enum InstructionValue {
    Str(u32),
    Num(fsize),
    Word(u32),
    Attr(Box<InstructionValue>, Box<InstructionValue>),
    Call(Box<InstructionValue>, Vec<InstructionValue>),
    Add(Box<InstructionValue>, Box<InstructionValue>),
    Sub(Box<InstructionValue>, Box<InstructionValue>),
    Div(Box<InstructionValue>, Box<InstructionValue>),
    Mult(Box<InstructionValue>, Box<InstructionValue>),
    Pow(Box<InstructionValue>, Box<InstructionValue>),
    Ternary(Box<InstructionValue>, Box<InstructionValue>, Box<InstructionValue>),
    Array(Vec<InstructionValue>),
    Dict(Vec<(u32, InstructionValue)>),
    Group(Box<InstructionValue>),
    Await(Box<InstructionValue>),
    Invert(Box<InstructionValue>),
    And(Box<InstructionValue>, Box<InstructionValue>),
    Or(Box<InstructionValue>, Box<InstructionValue>),
    // TODO(Scientific-Guy): Support for `in` keyword places rather only in for loops.
    In(Box<InstructionValue>, Box<InstructionValue>),
    Func(u32, Vec<u32>, Vec<u8>),
    Condition(Box<InstructionValue>, LogicalOperator, Box<InstructionValue>),
    True,
    False,
    Null
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Var(usize, u32, InstructionValue),
    Assign(usize, InstructionValue, u8, InstructionValue),
    Return(usize, InstructionValue),
    Value(usize, InstructionValue)
}

impl BytecodeReader {

    pub fn new(comp: BytecodeCompiler) -> Self {
        Self {
            bytes: comp.bytes,
            constants: comp.constants,
            len: 0,
            ci: 0,
            pos_map: comp.pos_map
        }
    }

    pub fn init(&mut self) -> Instruction {
        self.len = self.bytes.len();
        if self.len == 0 { std::process::exit(0) };
        self.parse_byte(self.bytes[self.ci])
    }

    pub fn parse_byte(&mut self, op: u8) -> Instruction {
        self.ci += 1;
        let instruction = match Opcode::from(op) {
            Opcode::Var => Instruction::Var(
                self.ci-1,
                self.get_len_based_constant_idx(),
                { 
                    self.ci += 1;
                    let op = Opcode::from(self.bytes[self.ci]);
                    self.ci += 1;
                    self.parse_value(op)
                }
            ),
            Opcode::Return => Instruction::Return(self.ci-1, {
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                self.parse_value(op)
            }),
            Opcode::Assign => Instruction::Assign(
                self.ci-1,
                {
                    let op = Opcode::from(self.bytes[self.ci]);
                    self.ci += 1;
                    self.parse_value(op)
                },
                {
                    self.ci += 1;
                    self.get_byte()
                },
                { 
                    self.ci += 1;
                    let op = Opcode::from(self.bytes[self.ci]);
                    self.ci += 1;
                    self.parse_value(op)
                }
            ),
            opcode => Instruction::Value(self.ci, self.parse_value(opcode))
        };

        self.ci += 1;
        instruction
    }

    pub fn parse_two_value(&mut self) -> (InstructionValue, InstructionValue) {
        (
            {
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                self.parse_value(op)
            },
            {
                self.ci += 1;
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                self.parse_value(op)
            }
        )
    }

    pub fn parse_value(&mut self, op: Opcode) -> InstructionValue {
        match op {
            Opcode::True => {
                self.ci -= 1;
                InstructionValue::True
            },
            Opcode::False => {
                self.ci -= 1;
                InstructionValue::False
            },
            Opcode::Null => {
                self.ci -= 1;
                InstructionValue::Null
            },
            Opcode::Num => InstructionValue::Num(match self.bytes.get(self.ci..self.ci + MAX_BYTES) {
                Some(bytes) => {
                    self.ci += MAX_BYTES - 1;
                    fsize::from_le_bytes(bytes.try_into().unwrap())
                },
                None => {
                    println!("CompilerError: Dislocated bytes.\n    Expected 7 bytes to generate a constant id but failed.");
                    std::process::exit(0);
                }
            }),
            Opcode::Str => InstructionValue::Str(self.get_byte() as u32),
            Opcode::StrLong => InstructionValue::Str(self.get_u32()),
            Opcode::Word => InstructionValue::Word(self.get_byte() as u32),
            Opcode::WordLong => InstructionValue::Word(self.get_u32()),
            Opcode::Attr => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Attr(Box::new(a), Box::new(b))
            },
            Opcode::Add => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Add(Box::new(a), Box::new(b))
            },
            Opcode::Sub => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Sub(Box::new(a), Box::new(b))
            },
            Opcode::Mult => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Mult(Box::new(a), Box::new(b))
            },
            Opcode::Div => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Div(Box::new(a), Box::new(b))
            },
            Opcode::Pow => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Pow(Box::new(a), Box::new(b))
            },
            Opcode::Func => {
                let name = self.get_len_based_constant_idx();
                self.ci += 1;
                let param_len = self.get_byte();
                self.ci += 1;
                let mut params = Vec::new();
                for _ in 0..param_len {
                    params.push(self.get_len_based_constant_idx());
                    self.ci += 1;
                }

                let mut chunk = Vec::new();
                while self.ci < self.len {
                    match Opcode::from(self.bytes[self.ci]) {
                        Opcode::FuncEnd => return InstructionValue::Func(name, params, chunk),
                        _ => chunk.push(self.bytes[self.ci])
                    }

                    self.ci += 1;
                }

                InstructionValue::Func(name, params, chunk)
            },
            Opcode::Call => InstructionValue::Call(
                {
                    let op = Opcode::from(self.bytes[self.ci]);
                    self.ci += 1;
                    Box::new(self.parse_value(op))
                }, 
                {
                    self.ci += 1;
                    let len = self.get_byte();
                    let mut params = Vec::new();

                    for _ in 0..len {
                        self.ci += 1;
                        params.push({
                            let op = Opcode::from(self.bytes[self.ci]);
                            self.ci += 1;
                            self.parse_value(op)
                        });
                    }

                    params
                }
            ),
            Opcode::Ternary => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Ternary(
                    Box::new(a),
                    Box::new(b),
                    Box::new({
                        self.ci += 1;
                        let op = Opcode::from(self.bytes[self.ci]);
                        self.ci += 1;
                        self.parse_value(op)
                    })
                )
            },
            Opcode::Array => {
                let len = self.get_u32();
                let mut arr = Vec::new();

                for _ in 0..len {
                    self.ci += 1;
                    arr.push({
                        let op = Opcode::from(self.bytes[self.ci]);
                        self.ci += 1;
                        self.parse_value(op)
                    });
                }

                InstructionValue::Array(arr)
            },
            Opcode::Dict => {
                let len = self.get_u32();
                let mut arr = Vec::new();

                for _ in 0..len {
                    arr.push({
                        self.ci += 1;

                        (self.get_len_based_constant_idx(), {
                            self.ci += 1;
                            let op = Opcode::from(self.bytes[self.ci]);
                            self.ci += 1;
                            self.parse_value(op)
                        })
                    });
                }

                InstructionValue::Dict(arr)
            },
            Opcode::Group => InstructionValue::Group({
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                Box::new(self.parse_value(op))
            }),
            Opcode::Await => InstructionValue::Await({
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                Box::new(self.parse_value(op))
            }),
            Opcode::Invert => InstructionValue::Invert(Box::new({
                let op = Opcode::from(self.bytes[self.ci]);
                self.ci += 1;
                self.parse_value(op)
            })),
            Opcode::And => {
                let (a, b) = self.parse_two_value();
                InstructionValue::And(Box::new(a), Box::new(b))
            },
            Opcode::Or => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Or(Box::new(a), Box::new(b))
            },
            Opcode::In => {
                let (a, b) = self.parse_two_value();
                InstructionValue::In(Box::new(a), Box::new(b))
            },
            Opcode::GreaterThan => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Condition(Box::new(a), LogicalOperator::GreaterThan, Box::new(b))
            },
            Opcode::LessThan => {
                let (a, b) = self.parse_two_value();
                InstructionValue::Condition(Box::new(a), LogicalOperator::LessThan, Box::new(b))
            },
            op => {
                println!("CompilerError: Dislocated bytes.\n    Detected an unexpected bytecode: {:?}", op);
                std::process::exit(0);
            }
        }
    }

    pub fn next(&mut self) -> Option<Instruction> {
        if (self.ci+1) < self.len {
            Some(self.parse_byte(self.bytes[self.ci]))
        } else {
            None
        }
    }

    pub fn get_len_based_constant_idx(&mut self) -> u32 {
        match Opcode::from(self.bytes[self.ci]) {
            Opcode::Short => {
                self.ci += 1;
                self.get_byte() as u32
            },
            Opcode::Long => {
                self.ci += 1;
                self.get_u32()
            },
            _ => 0
        }
    }

    pub fn get_byte(&mut self) -> u8 {
        match self.bytes.get(self.ci) {
            Some(byte) => *byte,
            None => {
                println!("CompilerError: Dislocated bytes.\n    Expected 1 byte but found no bytes.");
                std::process::exit(0);
            }
        }
    }

    pub fn get_u32(&mut self) -> u32 {
        match self.bytes.get(self.ci..self.ci + 4) {
            Some(bytes) => {
                self.ci += 3;
                u32::from_le_bytes(bytes.try_into().unwrap())
            },
            None => {
                println!("CompilerError: Dislocated bytes.\n    Expected 4 bytes to generate a u32 but failed.");
                std::process::exit(0);
            }
        }
    }

    pub fn get_position(&self, ci: usize) -> Position {
        let mut result = Position::default();
        for pos in self.pos_map.iter() {
            if pos.0 <= ci {
                result = pos.1;
            } else {
                break;
            }
        }

        result
    }

    pub fn get_constant(&self, id: usize) -> String {
        self.constants[id].clone()
    }

}