use std::io::{stdin, Read};

type MemoryRef<'a> = &'a mut Vec<usize>;
type PositionRef<'a> = &'a mut usize;

fn parse_input(input: &String) -> Vec<usize> {
    input
        .split(',')
        .map(|x| x.parse().expect("Failed to parse input"))
        .collect()
}

fn step(position: PositionRef) {
    *position += 4;
}

fn run_operation<F>(memory: MemoryRef, position: PositionRef, func: F)
where
    F: Fn(usize, usize) -> usize,
{
    let x = memory[memory[*position + 1]];
    let y = memory[memory[*position + 2]];
    let out = memory[*position + 3];
    memory[out] = func(x, y);
}

fn run(memory: MemoryRef, position: PositionRef) -> usize {
    let operation = memory[*position];
    match operation {
        1 => {
            // Add
            run_operation(memory, position, |x, y| x + y);
            step(position);
            run(memory, position)
        }
        2 => {
            // Multiply
            run_operation(memory, position, |x, y| x * y);
            step(position);
            run(memory, position)
        }
        99 => {
            // Halt
            memory[0]
        }
        _ => panic!("Invalid operation {} at position {}", operation, position),
    }
}

fn solve(input: &String) -> usize {
    let mut memory = parse_input(&input);
    let mut position = 0;

    memory[1] = 12;
    memory[2] = 2;

    run(&mut memory, &mut position)
}

fn main() {
    let mut input = String::new();

    stdin()
        .read_to_string(&mut input)
        .expect("Failed to read stdin");

    let solution = solve(&input);

    println!("{}", solution);
}
