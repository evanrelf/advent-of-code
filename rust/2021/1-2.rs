use std::{collections::VecDeque, io::Read};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = std::io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let depths: Vec<usize> = input.lines().map(|line| line.parse().unwrap()).collect();

    let depth_window_size = 3;

    let (increases_count, _) = depths.iter().fold(
        (0, VecDeque::new()),
        |(increases_count, mut depth_window), &depth| {
            let depth_window_is_full = depth_window.len() == depth_window_size;

            let previous_sum: usize = depth_window.iter().sum();

            depth_window.push_front(depth);
            depth_window.truncate(depth_window_size);

            let current_sum: usize = depth_window.iter().sum();

            let sum_is_larger = current_sum > previous_sum;

            (
                if depth_window_is_full && sum_is_larger {
                    increases_count + 1
                } else {
                    increases_count
                },
                depth_window,
            )
        },
    );

    println!("{}", increases_count);

    Ok(())
}
