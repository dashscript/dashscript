use std::io::{self, Write};
use crate::cli::command::Command;
use crate::lexer::parser::Lexer;
use crate::ast::main::AST;
use crate::bytecode::main::BytecodeCompiler;
use crate::bytecode::reader::{ BytecodeReader, Instruction };
use crate::vm::vm::VM;
use crate::vm::vmcore::builtin::inspect;

// TODO(Scientific-Guy): Make a better repl. This repl was initially made for testing cases.
pub fn start_repl(cli: &mut Command) {
    cli.flags.insert("repl".to_string(), String::new());
    let mut vm = VM::default("@repl".to_string(), cli.flags.clone());
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    vm.init_core();
    print!("Dashscript 1.0.0\nEnter Ctrl+C to exit.\n\n");
    stdout.flush().unwrap();

    loop {
        print!(">> ");
        stdout.flush().unwrap();
        let mut line = String::new();

        match stdin.read_line(&mut line) {
            Ok(_) => (),
            Err(e) => {
                println!("Failed reading the line {}", e);
                std::process::exit(0)
            }
        }

        if line.len() == 0 {
            continue;
        }

        let lexer = match Lexer::new(&"@repl".to_string(), &line) {
            Ok(lexer) => lexer,
            Err(message) => {
                println!("{}", message);
                continue;
            }
        };

        vm.pos_map.clear();
        vm.reader = BytecodeReader::new(
            BytecodeCompiler::new(match AST::new(&lexer) {
                Ok(ast) => ast,
                Err(err) => {
                    println!("{}", err);
                    continue;
                }
            })
        );

        let mut instruction = Some(vm.reader.init());
        while instruction.is_some() {
            if let Some(Instruction::Value(pos, ref val)) = instruction {
                match vm.execute_value(val.clone(), pos) {
                    Ok(val) => println!("{}", inspect(val, &mut vm)),
                    Err(e) => {
                        println!("{}", e);
                        break;
                    }
                }
            } else {
                match vm.execute_instruction(instruction.unwrap()) {
                    Ok(_) => println!("null"),
                    Err(e) => {
                        println!("{}", e);
                        break;
                    }
                }
            }
            
            instruction = vm.reader.next();
        }

        println!(" ");
    }
}