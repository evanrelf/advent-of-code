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

fn find_contiguous_summands(numbers: Vec<usize>, search_sum: usize) -> Option<Vec<usize>> {
    let length = numbers.len();

    for i in 0..length {
        for j in i..length {
            let iter = numbers.iter().skip(i).take(j - i);

            let iter_sum: usize = iter.clone().sum();

            if iter_sum == search_sum {
                return Some(iter.copied().collect());
            } else if iter_sum > search_sum {
                break;
            }
        }
    }

    None
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let numbers: Vec<usize> = input.lines().map(|line| line.parse().unwrap()).collect();

    let invalid_number = {
        let preamble_size = 25;

        let mut xmas = Xmas::new(preamble_size);

        let mut remaining_numbers = numbers.clone();

        remaining_numbers
            .drain(0..preamble_size)
            .for_each(|number| xmas.push(number));

        let mut invalid_number = None;

        for number in remaining_numbers {
            if xmas.is_valid(number) {
                xmas.push(number);
            } else {
                invalid_number = Some(number);
                break;
            }
        }

        match invalid_number {
            Some(n) => n,
            None => panic!("Couldn't find first invalid number"),
        }
    };

    println!("First invalid number: {}", invalid_number);

    match find_contiguous_summands(numbers, invalid_number) {
        Some(sequence) => {
            let min = sequence.iter().min().unwrap();
            let max = sequence.iter().max().unwrap();
            println!(
                "Sequence: {:#?}\nSmallest: {}\nLargest: {}\nSum: {}",
                sequence,
                min,
                max,
                min + max
            )
        }
        None => panic!("Failed to find sequence"),
    }

    Ok(())
}
