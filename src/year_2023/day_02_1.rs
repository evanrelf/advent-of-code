use anyhow::anyhow;
use winnow::{
    ascii::{digit1, line_ending},
    combinator::{alt, delimited, separated, terminated},
    prelude::*,
};

crate::aoc!();

struct Game {
    id: usize,
    cubes: Vec<CubeSet>,
}

#[derive(Default)]
struct CubeSet {
    red: usize,
    green: usize,
    blue: usize,
}

pub fn solve(mut input: &str) -> anyhow::Result<usize> {
    let games: Vec<_> = separated(1.., game, line_ending)
        .parse_next(&mut input)
        .map_err(|e| anyhow!(e))?;

    let is_possible = |game: &Game| {
        game.cubes
            .iter()
            .all(|CubeSet { red, green, blue }| *red <= 12 && *green <= 13 && *blue <= 14)
    };

    let solution = games
        .into_iter()
        .filter(is_possible)
        .map(|game| game.id)
        .sum();

    Ok(solution)
}

fn game(input: &mut &str) -> PResult<Game> {
    let id = delimited("Game ", usize, ": ").parse_next(input)?;
    let cubes = separated(1.., cube_set, "; ").parse_next(input)?;
    Ok(Game { id, cubes })
}

fn cube_set(input: &mut &str) -> PResult<CubeSet> {
    let red = terminated(usize, " red").map(|red| CubeSet {
        red,
        ..CubeSet::default()
    });

    let green = terminated(usize, " green").map(|green| CubeSet {
        green,
        ..CubeSet::default()
    });

    let blue = terminated(usize, " blue").map(|blue| CubeSet {
        blue,
        ..CubeSet::default()
    });

    let color = alt((red, green, blue));

    let colors: Vec<_> = separated(1.., color, ", ").parse_next(input)?;

    let cube_set = colors.into_iter().fold(CubeSet::default(), |x, y| CubeSet {
        red: x.red + y.red,
        green: x.green + y.green,
        blue: x.blue + y.blue,
    });

    Ok(cube_set)
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
            Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
            Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
            Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
            Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
            Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        "};
        assert_eq!(solve(input).unwrap(), 8);
    }
}
