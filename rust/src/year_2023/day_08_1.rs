use anyhow::{anyhow, Context as _};
use std::{collections::HashMap, str::FromStr};
use winnow::{
    ascii::line_ending,
    combinator::{dispatch, fail, repeat, separated, success},
    prelude::*,
    token::{any, one_of},
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let network = input.parse::<Network>()?;
    let mut here = Node(['A', 'A', 'A']);
    for (step, instruction) in network.instructions.iter().cycle().enumerate() {
        if here == Node(['Z', 'Z', 'Z']) {
            return Ok(step);
        }
        let (left, right) = network.paths.get(&here).context("Failed to move")?;
        match instruction {
            Instruction::Left => here = *left,
            Instruction::Right => here = *right,
        }
    }
    unreachable!()
}

struct Network {
    instructions: Vec<Instruction>,
    paths: HashMap<Node, (Node, Node)>,
}

impl FromStr for Network {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        network.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn network(input: &mut &str) -> PResult<Network> {
    let instructions = repeat(1.., instruction).parse_next(input)?;
    repeat(2, line_ending).parse_next(input)?;
    let paths: Vec<_> = separated(1.., path, line_ending).parse_next(input)?;
    repeat(0.., line_ending).parse_next(input)?;
    let paths = paths.into_iter().fold(HashMap::new(), |mut paths, path| {
        paths.insert(path.start, (path.left, path.right));
        paths
    });
    Ok(Network {
        instructions,
        paths,
    })
}

#[derive(Clone, Copy)]
enum Instruction {
    Left,
    Right,
}

fn instruction(input: &mut &str) -> PResult<Instruction> {
    dispatch! {any;
        'L' => success(Instruction::Left),
        'R' => success(Instruction::Right),
        _ => fail,
    }
    .parse_next(input)
}

struct Path {
    start: Node,
    left: Node,
    right: Node,
}

fn path(input: &mut &str) -> PResult<Path> {
    let start = node.parse_next(input)?;
    let _ = " = (".parse_next(input)?;
    let left = node.parse_next(input)?;
    let _ = ", ".parse_next(input)?;
    let right = node.parse_next(input)?;
    let _ = ")".parse_next(input)?;
    Ok(Path { start, left, right })
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Node([char; 3]);

fn node(input: &mut &str) -> PResult<Node> {
    let letters: Vec<_> = repeat(3, one_of('A'..='Z')).parse_next(input)?;
    Ok(Node([letters[0], letters[1], letters[2]]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE_1: &str = indoc! {"
        RL

        AAA = (BBB, CCC)
        BBB = (DDD, EEE)
        CCC = (ZZZ, GGG)
        DDD = (DDD, DDD)
        EEE = (EEE, EEE)
        GGG = (GGG, GGG)
        ZZZ = (ZZZ, ZZZ)
    "};

    const SAMPLE_2: &str = indoc! {"
        LLR

        AAA = (BBB, BBB)
        BBB = (AAA, ZZZ)
        ZZZ = (ZZZ, ZZZ)
    "};

    #[test]
    fn test_sample_1() {
        assert_eq!(solve(SAMPLE_1).unwrap(), 2);
    }

    #[test]
    fn test_sample_2() {
        assert_eq!(solve(SAMPLE_2).unwrap(), 6);
    }
}
