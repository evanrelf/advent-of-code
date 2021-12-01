use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = std::io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let depths: Vec<usize> = input.lines().map(|line| line.parse().unwrap()).collect();

    let (increases_count, _) = depths.iter().fold(
        (0, None),
        |(increases_count, previous_depth), current_depth| {
            (
                match previous_depth {
                    Some(previous_depth) if current_depth > previous_depth => increases_count + 1,
                    _ => increases_count,
                },
                Some(current_depth),
            )
        },
    );

    println!("{}", increases_count);

    Ok(())
}
