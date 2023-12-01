use anyhow::Context as _;

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let mut sum = 0;
    for line in input.lines() {
        let mut first = None;
        let mut last = None;
        for char in line.chars() {
            if char.is_ascii_digit() {
                if first.is_none() {
                    first = Some(char);
                } else {
                    last = Some(char);
                }
            }
        }
        let number_string = match (first, last) {
            (Some(first), Some(last)) => format!("{first}{last}"),
            (Some(first), None) => format!("{first}{first}"),
            (None, Some(_)) => unreachable!(),
            (None, None) => anyhow::bail!("Line lacks digits"),
        };
        let number = number_string
            .parse::<usize>()
            .context("Failed to parse number")?;
        sum += number;
    }
    Ok(sum)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test() {
        let input = indoc! {"
            1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet
        "};
        assert_eq!(solve(input).unwrap(), 142);
    }
}
