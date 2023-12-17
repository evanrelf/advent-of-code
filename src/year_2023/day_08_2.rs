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
    unreachable!()
}

fn path_iter(start: Node, network: &Network) -> impl Iterator<Item = (Step, Node)> {
    let mut here = start;

    network
        .instructions
        .iter()
        .fold(start, |node, instruction| {
            let (left, right) = network
                .paths
                .get(&here)
                .expect("Paths in puzzle input are valid");

            let node = match instruction {
                Instruction::Left => *left,
                Instruction::Right => *right,
            }

            (Step(step), node)
        })
        .take_while(move |(step, node)| *step == Step(0) && *node != start)
}

#[derive(Debug, PartialEq)]
struct Step(usize);

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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Node([char; 3]);

impl FromStr for Node {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        node.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn node(input: &mut &str) -> PResult<Node> {
    let letters: Vec<_> = repeat(3, one_of(('A'..='Z', '0'..='9'))).parse_next(input)?;
    Ok(Node([letters[0], letters[1], letters[2]]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        LR

        11A = (11B, XXX)
        11B = (XXX, 11Z)
        11Z = (11B, XXX)
        22A = (22B, XXX)
        22B = (22C, 22C)
        22C = (22Z, 22Z)
        22Z = (22B, 22B)
        XXX = (XXX, XXX)
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 2);
    }

    #[test]
    fn test_path_iter() {
        let network = SAMPLE.parse::<Network>().unwrap();

        let start = Node(['1', '1', 'A']);
        let expected: Vec<(Step, Node)> = vec!["11A", "11B", "11Z", "11B", "11Z", "11B", "11Z"]
            .into_iter()
            .enumerate()
            .map(|(step, node)| (Step(step), node.parse().unwrap()))
            .collect();
        let actual = path_iter(start, &network).take(7).collect::<Vec<_>>();
        assert_eq!(expected, actual);

        let start = Node(['2', '2', 'A']);
        let expected: Vec<(Step, Node)> = vec!["22A", "22B", "22C", "22Z", "22B", "22C", "22Z"]
            .into_iter()
            .enumerate()
            .map(|(step, node)| (Step(step), node.parse().unwrap()))
            .collect();
        let actual = path_iter(start, &network).take(7).collect::<Vec<_>>();
        assert_eq!(expected, actual);
    }
}
