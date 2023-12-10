use anyhow::anyhow;
use std::str::FromStr;
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{delimited, repeat, separated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let race = input.parse::<Race>()?;

    let solution = winning_strategies(race.time, race.distance).count();

    Ok(solution)
}

fn winning_strategies(race_time: usize, distance_record: usize) -> impl Iterator<Item = usize> {
    (1..race_time)
        .map(move |button_time| distance(race_time, button_time))
        .filter(move |distance| *distance > distance_record)
}

fn distance(race_time: usize, button_time: usize) -> usize {
    let speed = button_time;
    let travel_time = race_time - button_time;
    speed * travel_time
}

struct Race {
    time: usize,
    distance: usize,
}

impl FromStr for Race {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        race.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn race(input: &mut &str) -> PResult<Race> {
    let time = delimited(
        ("Time:", space1),
        separated(1.., digit1, space1)
            .map(|vec: Vec<_>| {
                vec.into_iter().fold(String::new(), |mut string, str| {
                    string.push_str(str);
                    string
                })
            })
            .try_map(|s| s.parse()),
        line_ending,
    )
    .parse_next(input)?;

    let distance = delimited(
        ("Distance:", space1),
        separated(1.., digit1, space1)
            .map(|vec: Vec<_>| {
                vec.into_iter().fold(String::new(), |mut string, str| {
                    string.push_str(str);
                    string
                })
            })
            .try_map(|s| s.parse()),
        repeat::<_, _, (), _, _>(0.., line_ending),
    )
    .parse_next(input)?;

    Ok(Race { time, distance })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        Time:      7  15   30
        Distance:  9  40  200
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 71503);
    }
}
