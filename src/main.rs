use clap::Parser as _;
use std::{io::Read as _, process::ExitCode};

#[derive(clap::Parser)]
struct Args {
    year: u16,
    day: u8,
    part: u8,
}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    let output = match (args.year, args.day, args.part) {
        (2023, 1, 1) => get_input()?, // TODO
        (year, day, part) => {
            eprintln!("No solution for {year} day {day} part {part}");
            return Ok(ExitCode::FAILURE);
        }
    };

    println!("{output}");

    Ok(ExitCode::SUCCESS)
}

fn get_input() -> anyhow::Result<String> {
    let mut stdin = std::io::stdin().lock();
    let mut string = String::new();
    stdin.read_to_string(&mut string)?;
    Ok(string)
}
