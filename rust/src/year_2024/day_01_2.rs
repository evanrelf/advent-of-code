use anyhow::anyhow;
use std::collections::HashMap;
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{opt, separated, separated_pair, terminated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let (lefts, rights) = lists.parse(input).map_err(|e| anyhow!(e.to_string()))?;
    let mut right_counts = HashMap::new();
    for right in rights {
        right_counts
            .entry(right)
            .and_modify(|count| *count += 1)
            .or_insert(1usize);
    }
    let similarity_score = lefts
        .iter()
        .map(|x| x * right_counts.get(x).copied().unwrap_or(0))
        .sum();
    Ok(similarity_score)
}

fn lists(input: &mut &str) -> PResult<(Vec<usize>, Vec<usize>)> {
    terminated(separated(1.., pair, line_ending), opt(line_ending))
        .parse_next(input)
        .map(|ps: Vec<(usize, usize)>| ps.into_iter().unzip())
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
        assert_eq!(solve(input).unwrap(), 31);
    }
}