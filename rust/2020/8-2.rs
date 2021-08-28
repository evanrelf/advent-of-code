// use rayon::prelude::*;
use std::collections::HashSet;
use std::io::{self, Read};
use std::str::FromStr;

#[derive(Clone)]
struct Program {
    state: ProgramState,
    accumulator: i64,
    program_counter: usize,
    already_run: HashSet<usize>,
    instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, PartialEq)]
enum ProgramState {
    Running,
    InfiniteLoop,
    Finished,
}

impl Program {
    fn load(instructions: Vec<Instruction>) -> Program {
        Program {
            state: ProgramState::Running,
            accumulator: 0,
            program_counter: 0,
            already_run: HashSet::new(),
            instructions,
        }
    }

    fn flip(&mut self, position: usize) {
        match self.instructions.get(position) {
            Some(Instruction::Jmp(n)) => self.instructions[position] = Instruction::Nop(*n),
            Some(Instruction::Nop(n)) => self.instructions[position] = Instruction::Jmp(*n),
            Some(_) => {}
            None => panic!(
                "Attempted to flip nonexistent instruction (at {})",
                position
            ),
        }
    }

    fn step(&mut self) {
        let instruction = {
            match self.instructions.get(self.program_counter) {
                Some(i) => i,
                None if self.program_counter == self.instructions.len() => {
                    self.state = ProgramState::Finished;
                    return;
                }
                None => panic!(
                    "Program counter referenced nonexistent instruction (at {})",
                    self.program_counter
                ),
            }
        };

        if self.already_run.contains(&self.program_counter) {
            self.state = ProgramState::InfiniteLoop;
        } else {
            self.already_run.insert(self.program_counter);

            match instruction {
                Instruction::Acc(n) => {
                    self.accumulator += n;
                    self.program_counter += 1;
                }
                Instruction::Jmp(n) => {
                    let old = self.program_counter as i64;
                    let new = old + n;
                    self.program_counter = new as usize;
                }
                Instruction::Nop(_) => self.program_counter += 1,
            }
        }
    }

    fn run(&mut self) {
        while self.state == ProgramState::Running {
            self.step();
        }
    }
}

#[derive(Clone, PartialEq)]
enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
}

impl Instruction {
    fn is_flippable(&self) -> bool {
        match self {
            Instruction::Acc(_) => false,
            Instruction::Jmp(_) => true,
            Instruction::Nop(0) => false,
            Instruction::Nop(_) => true,
        }
    }
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(string: &str) -> Result<Instruction, String> {
        let words: Vec<&str> = string.split(' ').collect();

        let operation = words
            .get(0)
            .ok_or("Failed to parse instruction operation")?;

        let argument = {
            let (sign, number_str) = words
                .get(1)
                .ok_or("Failed to parse instruction argument")?
                .split_at(1);

            let number = number_str
                .parse()
                .map_err(|_| "Failed to parse instruction argument number")?;

            match sign {
                "+" => number,
                "-" => number * -1,
                _ => return Err("Failed to parse instruction argument sign".to_string()),
            }
        };

        match operation {
            &"acc" => Ok(Instruction::Acc(argument)),
            &"jmp" => Ok(Instruction::Jmp(argument)),
            &"nop" => Ok(Instruction::Nop(argument)),
            invalid => Err(format!("Invalid instruction '{}'", invalid)),
        }
    }
}

fn brute_force(instructions: Vec<Instruction>) -> i64 {
    let flippable_indices: Vec<usize> = instructions
        .iter()
        .enumerate()
        .filter(|(_, instruction)| instruction.is_flippable())
        .map(|(index, _)| index)
        .collect();

    let default_program = Program::load(instructions);

    let possible_programs: Vec<Program> = flippable_indices
        .iter()
        .map(|index| {
            let mut program = default_program.clone();
            program.flip(*index);
            program
        })
        .collect();

    // I couldn't figure out how to parallelize this with `rayon`, because I was fighting with the
    // borrow checker. But it's extremely quick even when run serially, so it doesn't matter.
    for mut program in possible_programs {
        program.run();
        if program.state == ProgramState::Finished {
            return program.accumulator;
        }
    }

    panic!("None of the programs finished");
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let instructions: Vec<_> = input.lines().map(|line| line.parse().unwrap()).collect();

    let solution = brute_force(instructions);

    println!("Solution: {}", solution);

    Ok(())
}
