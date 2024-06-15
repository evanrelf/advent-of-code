use anyhow::anyhow;
use std::iter::zip;
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{alt, opt, preceded, separated, terminated},
    prelude::*,
};

crate::aoc!();

fn solve(input: &str) -> anyhow::Result<isize> {
    let report = report.parse(input).map_err(|e| anyhow!(e.to_string()))?;
    Ok(report.into_iter().map(|history| predict(&history)).sum())
}

fn predict(values: &[isize]) -> isize {
    if let Some(next) = next_sequence(values) {
        let right = values[0];
        let below = predict(&next);
        right - below
    } else {
        0
    }
}

fn next_sequence(values: &[isize]) -> Option<Vec<isize>> {
    if values.iter().all(|x| *x == 0) {
        return None;
    }

    Some(
        zip(values.iter(), values.iter().skip(1))
            .map(|(l, r)| r - l)
            .collect(),
    )
}

type Report = Vec<History>;

type History = Vec<isize>;

fn report(input: &mut &str) -> PResult<Report> {
    terminated(separated(1.., history, line_ending), opt(line_ending)).parse_next(input)
}

fn history(input: &mut &str) -> PResult<History> {
    separated(1.., isize, space1).parse_next(input)
}

fn isize(input: &mut &str) -> PResult<isize> {
    let negative_usize = preceded('-', usize).map(|n| -n);
    alt((negative_usize, usize)).parse_next(input)
}

fn usize(input: &mut &str) -> PResult<isize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        0 3 6 9 12 15
        1 3 6 10 15 21
        10 13 16 21 30 45
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 2);
    }
}
