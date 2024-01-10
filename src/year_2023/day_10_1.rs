use anyhow::anyhow;
use flagset::{flags, FlagSet};
use winnow::{
    ascii::line_ending,
    combinator::{dispatch, fail, opt, repeat, separated, success, terminated},
    prelude::*,
    token::any,
};

crate::aoc!();

fn solve(input: &str) -> anyhow::Result<usize> {
    let grid = grid.parse(input).map_err(|e| anyhow!(e.to_string()))?;
    Ok(42)
}

struct Grid(Vec<Vec<Tile>>);

fn grid(input: &mut &str) -> PResult<Grid> {
    terminated(
        separated(1.., repeat::<_, _, Vec<_>, _, _>(1.., tile), line_ending),
        opt(line_ending),
    )
    .map(Grid)
    .parse_next(input)
}

flags! {
    enum Direction: u8 {
        North,
        South,
        East,
        West,
    }
}

#[derive(Clone, Copy)]
enum Tile {
    Pipe(FlagSet<Direction>),
    Ground,
    Start,
}

fn tile(input: &mut &str) -> PResult<Tile> {
    dispatch! {any;
        '|' => success(Tile::Pipe(Direction::North | Direction::South)),
        '-' => success(Tile::Pipe(Direction::East | Direction::West)),
        'L' => success(Tile::Pipe(Direction::North | Direction::East)),
        'J' => success(Tile::Pipe(Direction::North | Direction::West)),
        '7' => success(Tile::Pipe(Direction::South | Direction::West)),
        'F' => success(Tile::Pipe(Direction::South | Direction::East)),
        '.' => success(Tile::Ground),
        'S' => success(Tile::Start),
        _ => fail,
    }
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE_1: &str = indoc! {"
        .....
        .S-7.
        .|.|.
        .L-J.
        .....
    "};

    const SAMPLE_2: &str = indoc! {"
        -L|F7
        7S-7|
        L|7||
        -L-J|
        L|-JF
    "};

    const SAMPLE_3: &str = indoc! {"
        ..F7.
        .FJ|.
        SJ.L7
        |F--J
        LJ...
    "};

    const SAMPLE_4: &str = indoc! {"
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE_1).unwrap(), 4);
        assert_eq!(solve(SAMPLE_2).unwrap(), 4);
        assert_eq!(solve(SAMPLE_3).unwrap(), 8);
        assert_eq!(solve(SAMPLE_4).unwrap(), 8);
    }
}
