#[rustfmt::skip]
pub mod year_2023 { automod::dir!(pub "src/year_2023/"); }

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
        (2023, 1, 1) => year_2023::day_01_1::solve(&get_input()?)?,
        (2023, 1, 2) => year_2023::day_01_2::solve(&get_input()?)?,
        (2023, 2, 1) => year_2023::day_02_1::solve(&get_input()?)?,
        (2023, 2, 2) => year_2023::day_02_2::solve(&get_input()?)?,
        (2023, 3, 1) => year_2023::day_03_1::solve(&get_input()?)?,
        (2023, 3, 2) => year_2023::day_03_2::solve(&get_input()?)?,
        (2023, 4, 1) => year_2023::day_04_1::solve(&get_input()?)?,
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
