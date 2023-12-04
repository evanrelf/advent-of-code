pub fn solve(input: &str) -> anyhow::Result<usize> {
    Ok(42)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_sample() {
        let input = indoc! {"
            467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..
        "};
        assert_eq!(solve(input).unwrap(), 467_835);
    }
}
