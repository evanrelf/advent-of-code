mod solution;
pub(crate) use solution::aoc;

#[rustfmt::skip]
mod year_2023 { automod::dir!(pub "src/year_2023/"); }
#[rustfmt::skip]
mod year_2024 { automod::dir!(pub "src/year_2024/"); }

use crate::solution::SOLUTIONS;
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

    for solution in SOLUTIONS {
        let year = solution.year == args.year;
        let day = solution.day == args.day;
        let part = solution.part == args.part;

        if year && day && part {
            let input = get_input()?;
            let solve = solution.solve.lock().unwrap().take().unwrap();
            let output = solve(&input)?;
            println!("{output}");
            return Ok(ExitCode::SUCCESS);
        }
    }

    eprintln!(
        "No solution for year {} day {} part {}",
        args.year, args.day, args.part
    );
    Ok(ExitCode::FAILURE)
}

fn get_input() -> anyhow::Result<String> {
    let mut stdin = std::io::stdin().lock();
    let mut string = String::new();
    stdin.read_to_string(&mut string)?;
    Ok(string)
}
