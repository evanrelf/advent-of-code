use anyhow::anyhow;
use std::{
    collections::HashMap,
    str::FromStr,
    sync::{
        atomic::{AtomicU8, Ordering},
        Arc, Barrier,
    },
    thread,
};
use winnow::{
    ascii::line_ending,
    combinator::{dispatch, fail, repeat, separated, success},
    prelude::*,
    token::{any, one_of},
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let network = input.parse::<Network>()?;

    let starts = network.paths.keys().filter(|Node(node)| node[2] == b'A');

    let thread_count = starts.clone().count();

    assert!(thread_count < 20, "too many threads");

    let barrier = Arc::new(Barrier::new(thread_count + 1));

    let done_counter = AtomicU8::new(0);

    thread::scope(|scope| {
        for start in starts {
            scope.spawn(|| {
                for Node(node) in path_iter(*start, &network) {
                    if node[2] == b'Z' {
                        done_counter.fetch_add(1, Ordering::SeqCst);
                    }
                    barrier.wait();

                    let done_count = done_counter.load(Ordering::SeqCst);
                    let done = usize::from(done_count) == thread_count;
                    barrier.wait();

                    // Main thread resetting count
                    barrier.wait();

                    if done {
                        return;
                    }
                }
            });
        }

        let mut step = 0;

        loop {
            // Worker threads adding to count
            barrier.wait();

            let done_count = done_counter.load(Ordering::SeqCst);
            let done = usize::from(done_count) == thread_count;
            barrier.wait();

            println!("step: {step}, done: {done_count}/{thread_count}");
            done_counter.store(0, Ordering::SeqCst);
            barrier.wait();

            if done {
                return Ok(step);
            }
            step += 1;
        }
    })
}

fn path_iter(start: Node, network: &Network) -> impl Iterator<Item = Node> + '_ {
    network
        .instructions
        .iter()
        .cycle()
        .scan(start, move |here, instruction| {
            let node = *here;

            let (left, right) = network
                .paths
                .get(&node)
                .expect("Puzzle input has valid paths");

            *here = match instruction {
                Instruction::Left => *left,
                Instruction::Right => *right,
            };

            Some(node)
        })
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

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
struct Node([u8; 3]);

impl FromStr for Node {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        node.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn node(input: &mut &str) -> PResult<Node> {
    let chars: Vec<_> = repeat(3, one_of((b'A'..=b'Z', b'0'..=b'9'))).parse_next(input)?;
    Ok(Node([
        u8::try_from(chars[0]).expect("Parsed chars are ASCII"),
        u8::try_from(chars[1]).expect("Parsed chars are ASCII"),
        u8::try_from(chars[2]).expect("Parsed chars are ASCII"),
    ]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use std::iter::zip;

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
        assert_eq!(solve(SAMPLE).unwrap(), 6);
    }

    #[test]
    fn test_path_iter() {
        let network = SAMPLE.parse::<Network>().unwrap();

        let start = Node([b'1', b'1', b'A']);
        let expected = vec!["11A", "11B", "11Z", "11B", "11Z", "11B", "11Z"]
            .into_iter()
            .map(|node| node.parse::<Node>().unwrap());
        let actual = path_iter(start, &network);
        assert!(zip(expected, actual).all(|(e, a)| e == a));

        let start = Node([b'2', b'2', b'A']);
        let expected = vec!["22A", "22B", "22C", "22Z", "22B", "22C", "22Z"]
            .into_iter()
            .map(|node| node.parse::<Node>().unwrap());
        let actual = path_iter(start, &network);
        assert!(zip(expected, actual).all(|(e, a)| e == a));
    }
}
