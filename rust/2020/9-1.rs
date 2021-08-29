mod xmas {
    use std::collections::VecDeque;

    pub struct Xmas {
        preamble_size: usize,
        numbers: VecDeque<usize>,
    }

    impl Xmas {
        pub fn new(preamble_size: usize) -> Xmas {
            Xmas {
                preamble_size,
                numbers: VecDeque::new(),
            }
        }

        pub fn push(&mut self, number: usize) {
            self.numbers.push_back(number);
            if self.numbers.len() > self.preamble_size {
                self.numbers.pop_front();
            }
        }

        pub fn is_valid(&self, number: usize) -> bool {
            let length = self.numbers.len();

            for i in 0..length {
                for j in i..length {
                    if self.numbers[i] + self.numbers[j] == number {
                        return true;
                    }
                }
            }

            false
        }
    }
}

use crate::xmas::*;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let mut numbers: Vec<usize> = input.lines().map(|line| line.parse().unwrap()).collect();

    let preamble_size = 25;

    let mut xmas = Xmas::new(preamble_size);

    numbers
        .drain(0..preamble_size)
        .for_each(|number| xmas.push(number));

    for number in numbers {
        if xmas.is_valid(number) {
            xmas.push(number);
        } else {
            println!("Invalid: {}", number);
            break;
        }
    }

    Ok(())
}
