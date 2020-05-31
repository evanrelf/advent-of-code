use std::io::Read;

fn calculate_fuel(mass: i32) -> i32 {
    ((mass as f32 / 3.0).floor() - 2.0) as i32
}

fn solve(input: &String) -> i32 {
    input
        .lines()
        .map(|line| {
            let parsed = line.parse().expect("Failed to parse line");
            calculate_fuel(parsed)
        })
        .sum()
}

fn main() {
    let mut input = String::new();

    std::io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read stdin");

    println!("{}", solve(&input))
}
