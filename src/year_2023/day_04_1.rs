use anyhow::anyhow;
use std::str::FromStr;
use winnow::{
    ascii::{digit1, space1},
    combinator::separated,
    prelude::*,
};

crate::aoc!(2023, 4, 1, solve);

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let cards = input
        .lines()
        .map(str::parse::<Card>)
        .collect::<Result<Vec<_>, _>>()?;
    let points = cards
        .into_iter()
        .map(|card| score(&card.winning_numbers()))
        .sum();
    Ok(points)
}

fn score(winning_numbers: &[usize]) -> usize {
    let count = winning_numbers.len();
    match count {
        0 => 0,
        n => 2usize.pow(u32::try_from(n - 1).unwrap()),
    }
}

#[derive(Debug, PartialEq)]
struct Card {
    id: usize,
    win: Vec<usize>,
    have: Vec<usize>,
}

impl Card {
    fn winning_numbers(&self) -> Vec<usize> {
        let mut win = self.win.clone();
        win.sort_unstable();
        self.have
            .clone()
            .into_iter()
            .filter(|have| win.binary_search(have).is_ok())
            .collect()
    }
}

impl FromStr for Card {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        card.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn card(input: &mut &str) -> PResult<Card> {
    let _ = ("Card", space1).parse_next(input)?;
    let id = usize.parse_next(input)?;
    let _ = (":", space1).parse_next(input)?;
    let win = separated(1.., usize, space1).parse_next(input)?;
    let _ = (space1, "|", space1).parse_next(input)?;
    let have = separated(1.., usize, space1).parse_next(input)?;
    Ok(Card { id, win, have })
}

fn usize(input: &mut &str) -> PResult<usize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 13);
    }

    #[test]
    fn test_card_parse() {
        assert_eq!(
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                .parse::<Card>()
                .unwrap(),
            Card {
                id: 3,
                win: vec![1, 21, 53, 59, 44],
                have: vec![69, 82, 63, 72, 16, 21, 14, 1],
            }
        );
    }

    #[test]
    fn test_score() {
        assert_eq!(score(&[0; 0]), 0);
        assert_eq!(score(&[0; 1]), 1);
        assert_eq!(score(&[0; 2]), 2);
        assert_eq!(score(&[0; 4]), 8);
    }
}
