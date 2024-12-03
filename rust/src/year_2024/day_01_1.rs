use anyhow::anyhow;
use std::iter::zip;
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{opt, separated, separated_pair, terminated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let (mut lefts, mut rights) = lists.parse(input).map_err(|e| anyhow!(e.to_string()))?;
    lefts.sort_unstable();
    rights.sort_unstable();
    let total_distance = zip(lefts, rights)
        .map(|(left, right)| distance(left, right))
        .sum();
    Ok(total_distance)
}

fn distance(left: usize, right: usize) -> usize {
    if left > right {
        left - right
    } else {
        right - left
    }
}

fn lists(input: &mut &str) -> PResult<(Vec<usize>, Vec<usize>)> {
    terminated(separated(1.., pair, line_ending), opt(line_ending))
        .parse_next(input)
        .map(|x: Vec<(usize, usize)>| x.into_iter().unzip())
}

fn pair(input: &mut &str) -> PResult<(usize, usize)> {
    separated_pair(usize, space1, usize).parse_next(input)
}

fn usize(input: &mut &str) -> PResult<usize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test() {
        let input = indoc! {"
            3   4
            4   3
            2   5
            1   3
            3   9
            3   3
        "};
        assert_eq!(solve(input).unwrap(), 11);
    }
}
