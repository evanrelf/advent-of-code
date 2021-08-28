use std::collections::HashSet;
use std::io::{self, Read};
use std::str::FromStr;

struct Program {
    halt: bool,
    accumulator: i64,
    program_counter: usize,
    already_run: HashSet<usize>,
    instructions: Vec<Instruction>,
}

impl Program {
    fn load(instructions: Vec<Instruction>) -> Program {
        Program {
            halt: false,
            accumulator: 0,
            program_counter: 0,
            already_run: HashSet::new(),
            instructions,
        }
    }

    fn step(&mut self) {
        let instruction = &self.instructions.get(self.program_counter).expect(&format!(
            "Program counter referenced nonexistent instruction (at {})",
            self.program_counter
        ));

        if self.already_run.contains(&self.program_counter) {
            self.halt = true;
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

    fn run(&mut self) -> i64 {
        while !self.halt {
            self.step();
        }
        self.accumulator
    }
}

enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let instructions: Vec<_> = input.lines().map(|line| line.parse().unwrap()).collect();

    let mut program = Program::load(instructions);

    let solution = program.run();

    println!("Solution: {}", solution);

    Ok(())
}
