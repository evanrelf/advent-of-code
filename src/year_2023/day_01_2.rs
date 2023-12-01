use anyhow::Context as _;
use winnow::{combinator::alt, prelude::*, token::one_of};

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let mut sum = 0;
    for line in input.lines() {
        let mut first = None;
        let mut last = None;
        let mut input = line;
        while !input.is_empty() {
            let Ok(char) = digit.parse_next(&mut input) else {
                let Some((next_char_index, _)) = input.char_indices().nth(1) else {
                    break;
                };
                input = input
                    .get(next_char_index..)
                    .expect("index is valid because it came from `char_indices`");
                continue;
            };
            if first.is_none() {
                first = Some(char);
            } else {
                last = Some(char);
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

fn digit(input: &mut &str) -> PResult<char> {
    alt((
        one_of('1'..='9'),
        "one".value('1'),
        "two".value('2'),
        "three".value('3'),
        "four".value('4'),
        "five".value('5'),
        "six".value('6'),
        "seven".value('7'),
        "eight".value('8'),
        "nine".value('9'),
    ))
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test() {
        let input = indoc! {"
            two1nine
            eightwothree
            abcone2threexyz
            xtwone3four
            4nineeightseven2
            zoneight234
            7pqrstsixteen
        "};
        assert_eq!(solve(input).unwrap(), 281);
    }
}
