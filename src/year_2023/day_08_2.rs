use anyhow::{anyhow, Context as _};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    str::FromStr,
    sync::Arc,
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

    let starts = network
        .paths
        .keys()
        .filter(|Node(node)| node[2] == 'A')
        .copied()
        .collect::<Vec<_>>();

    assert!(starts.len() < 20, "too many threads");

    let network = Arc::new(network);

    let (end_tx, end_rx) = flume::unbounded::<(WorkerId, Step, bool)>();

    for (worker_id, start) in starts.into_iter().enumerate() {
        let worker = Worker {
            id: WorkerId(worker_id),
            network: Arc::clone(&network),
            start,
            end_tx: end_tx.clone(),
        };
        thread::spawn(move || run_worker(&worker).unwrap());
    }

    let solution = run_supervisor(end_rx);

    Ok(solution)
}

fn run_supervisor(end_rx: flume::Receiver<(WorkerId, Step, bool)>) -> usize {
    let worker_count = end_rx.sender_count();

    let mut work: BTreeMap<Step, HashSet<WorkerId>> = BTreeMap::new();

    while let Ok((worker_id, step, is_end)) = end_rx.recv() {
        if is_end {
            work.remove(&step);
            continue;
        }

        // Record the work
        work.entry(step)
            .and_modify(|worker_ids| {
                assert!(
                    !worker_ids.contains(&worker_id),
                    "Worker sent duplicate message"
                );
                worker_ids.insert(worker_id);
            })
            .or_insert_with(|| {
                let mut worker_ids = HashSet::new();
                worker_ids.insert(worker_id);
                worker_ids
            });

        // Get the earliest step we're still considering
        let first_entry = work
            .first_entry()
            .expect("At least one entry exists because we just inserted one");

        // Check if the workers synced up / reached consensus at this ending
        if first_entry.get().len() == worker_count {
            drop(end_rx);
            panic!("solution: {}", first_entry.key().0);
            return first_entry.key().0;
        }
    }

    unreachable!("Workers don't stop until after supervisor has returned");
}

struct Worker {
    id: WorkerId,
    network: Arc<Network>,
    start: Node,
    end_tx: flume::Sender<(WorkerId, Step, bool)>,
}

fn run_worker(worker: &Worker) -> anyhow::Result<()> {
    let mut here = worker.start;

    for (step, instruction) in worker.network.instructions.iter().cycle().enumerate() {
        let is_end = here.0[2] == 'Z';

        if worker.end_tx.send((worker.id, Step(step), is_end)).is_err() {
            return Ok(());
        }

        let (left, right) = worker.network.paths.get(&here).context("Failed to move")?;

        match instruction {
            Instruction::Left => here = *left,
            Instruction::Right => here = *right,
        }
    }

    unreachable!("Iterator cycles indefinitely");
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct WorkerId(usize);

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
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

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Node([char; 3]);

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
        assert_eq!(solve(SAMPLE).unwrap(), 6);
    }
}
