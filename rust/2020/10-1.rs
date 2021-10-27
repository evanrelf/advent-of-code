use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = std::io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let solution = solve(input);

    println!("Solution: {}", solution);

    Ok(())
}

fn solve(input: String) -> usize {
    let bag_adapters: Vec<usize> = {
        let mut vec: Vec<usize> = input.lines().map(|line| line.parse().unwrap()).collect();
        vec.sort();
        vec
    };

    let builtin_adapter = {
        let highest_rated = bag_adapters.iter().max().unwrap();
        highest_rated + 3
    };

    let adapters = {
        let mut xs = bag_adapters.clone();
        xs.push(builtin_adapter);
        xs
    };

    let (ones, threes) = adapters
        .iter()
        .fold((0, (0, 0)), |(prev, (ones, threes)), adapter| {
            (
                *adapter,
                match adapter - prev {
                    1 => (ones + 1, threes),
                    3 => (ones, threes + 1),
                    _ => panic!("NOOO"),
                },
            )
        })
        .1;

    ones * threes
}

#[test]
fn test_10_1() {
    let input = r#"
16
10
15
5
1
11
7
19
6
12
4
"#
    .trim()
    .to_string();

    assert!(solve(input) == 7 * 5)
}
