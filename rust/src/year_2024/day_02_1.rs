use anyhow::anyhow;
use std::cmp::Ordering;
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{alt, opt, preceded, separated, terminated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let reports = reports.parse(input).map_err(|e| anyhow!(e.to_string()))?;
    let safe_count = reports.iter().fold(
        0,
        |count, report| if is_safe(report) { count + 1 } else { count },
    );
    Ok(safe_count)
}

fn is_safe(report: &[isize]) -> bool {
    let mut previous_direction: Option<Direction> = None;
    let mut previous_level: Option<isize> = None;
    for level in report {
        if let Some(previous_level) = previous_level {
            let delta = (level - previous_level).abs();
            if !(1..=3).contains(&delta) {
                // unsafe because adjacent levels differ too much
                return false;
            }
            let direction = match previous_level.cmp(level) {
                Ordering::Less => Direction::Increasing,
                Ordering::Greater => Direction::Decreasing,
                Ordering::Equal => panic!("unreachable; we already checked for zero delta"),
            };
            if let Some(previous_direction) = previous_direction {
                if direction != previous_direction {
                    // unsafe because changed direction
                    return false;
                }
            } else {
                previous_direction = Some(direction);
            }
        }
        previous_level = Some(*level);
    }
    true
}

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Increasing,
    Decreasing,
}

fn reports(input: &mut &str) -> PResult<Vec<Vec<isize>>> {
    terminated(separated(1.., report, line_ending), opt(line_ending)).parse_next(input)
}

fn report(input: &mut &str) -> PResult<Vec<isize>> {
    separated(1.., number, space1).parse_next(input)
}

fn number(input: &mut &str) -> PResult<isize> {
    alt((negative_number, positive_number)).parse_next(input)
}

fn negative_number(input: &mut &str) -> PResult<isize> {
    preceded('-', positive_number).map(|n| -n).parse_next(input)
}

fn positive_number(input: &mut &str) -> PResult<isize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test() {
        let input = indoc! {"
            7 6 4 2 1
            1 2 7 8 9
            9 7 6 2 1
            1 3 2 4 5
            8 6 4 4 1
            1 3 6 7 9
        "};
        assert_eq!(solve(input).unwrap(), 2);
    }
}
