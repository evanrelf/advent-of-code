use anyhow::anyhow;
use std::{iter::zip, str::FromStr};
use winnow::{
    ascii::{digit1, line_ending, space1},
    combinator::{delimited, repeat, separated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let races = input.parse::<Races>()?;

    let solution = zip(races.times, races.distances)
        .map(|(race_time, distance_record)| winning_strategies(race_time, distance_record))
        .map(Iterator::count)
        .product::<usize>();

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

struct Races {
    times: Vec<usize>,
    distances: Vec<usize>,
}

impl FromStr for Races {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        races.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn races(input: &mut &str) -> PResult<Races> {
    let times: Vec<_> = delimited(
        ("Time:", space1),
        separated(1.., usize, space1),
        line_ending,
    )
    .parse_next(input)?;

    let distances: Vec<_> = delimited(
        ("Distance:", space1),
        separated(1.., usize, space1),
        repeat::<_, _, (), _, _>(0.., line_ending),
    )
    .parse_next(input)?;

    assert_eq!(times.len(), distances.len());

    Ok(Races { times, distances })
}

fn usize(input: &mut &str) -> PResult<usize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
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
        assert_eq!(solve(SAMPLE).unwrap(), 288);
    }
}
