use clap::Parser as _;
use std::process::ExitCode;

#[derive(clap::Parser)]
struct Args {
    year: u16,
    day: u8,
    part: u8,
}

fn main() -> ExitCode {
    let args = Args::parse();

    match (args.year, args.day, args.part) {
        (year, day, part) => {
            eprintln!("No solution for {year} day {day} part {part}");
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
