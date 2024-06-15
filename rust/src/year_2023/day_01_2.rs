use anyhow::Context as _;
use winnow::{combinator::alt, prelude::*, token::one_of};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let mut sum = 0;
    for line in input.lines() {
        let mut first = None;
        let mut last = None;
        let mut input = line;
        while !input.is_empty() {
            let result = digit.parse_peek(input);
            let mut indices = input.char_indices();
            match (indices.next(), indices.next()) {
                (Some(_), Some((next_char_index, _))) => {
                    input = input
                        .get(next_char_index..)
                        .expect("index is valid because it came from `char_indices`");
                    let Ok((_, char)) = result else {
                        continue;
                    };
                    if first.is_none() {
                        first = Some(char);
                    } else {
                        last = Some(char);
                    }
                }
                (Some(_), None) => {
                    let Ok((_, char)) = result else {
                        break;
                    };
                    if first.is_none() {
                        first = Some(char);
                    } else {
                        last = Some(char);
                    }
                    break;
                }
                (None, Some(_)) => unreachable!(),
                (None, None) => break,
            }
        }
        let number_string = match (first, last) {
            (Some(first), Some(last)) => format!("{first}{last}"),
            (Some(first), None) => format!("{first}{first}"),
            (None, Some(_)) => unreachable!(),
            (None, None) => anyhow::bail!("Line lacks digits: {line}"),
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
    fn test_sample() {
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

    #[test]
    fn test_overlap() {
        let input = "qccnqvfnkkkvsixktsixnine1twoneq";
        assert_eq!(solve(input).unwrap(), 61);
    }
}
