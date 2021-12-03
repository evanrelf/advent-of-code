use std::io::Read;

type Bit = usize;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = std::io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let numbers: Vec<Vec<Bit>> = input
        .lines()
        .map(|binary_number| {
            binary_number
                .chars()
                .map(|c| c.to_string().parse().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let gamma_rate = binary_to_decimal(&calculate_gamma_rate(&numbers));

    let epsilon_rate = binary_to_decimal(&calculate_epsilon_rate(&numbers));

    let power_consumption = gamma_rate * epsilon_rate;

    println!("{}", power_consumption);

    Ok(())
}

fn calculate_gamma_rate(binary_numbers: &Vec<Vec<Bit>>) -> Vec<Bit> {
    let binary_numbers_count = binary_numbers.len();

    let mut ones_counts = vec![0; binary_numbers[0].len()];

    for binary_number in binary_numbers {
        let mut index = 0;
        for bit in binary_number {
            ones_counts[index] += bit;
            index += 1;
        }
    }

    ones_counts
        .iter()
        .map(|&ones_count| {
            if ones_count > (binary_numbers_count / 2) {
                1
            } else {
                0
            }
        })
        .collect()
}

fn calculate_epsilon_rate(binary_numbers: &Vec<Vec<Bit>>) -> Vec<Bit> {
    let binary_numbers_count = binary_numbers.len();

    let mut ones_counts = vec![0; binary_numbers[0].len()];

    for binary_number in binary_numbers {
        let mut index = 0;
        for bit in binary_number {
            ones_counts[index] += bit;
            index += 1;
        }
    }

    ones_counts
        .iter()
        .map(|&ones_count| {
            if ones_count > (binary_numbers_count / 2) {
                0
            } else {
                1
            }
        })
        .collect()
}

fn binary_to_decimal(binary_number: &Vec<Bit>) -> usize {
    let mut decimal_number = 0;
    let mut power = 0;
    for bit in binary_number.iter().rev() {
        decimal_number += bit * 2_usize.pow(power);
        power += 1;
    }
    decimal_number
}

#[test]
fn test_binary_to_decimal() {
    assert_eq!(binary_to_decimal(&vec![0]), 0);
    assert_eq!(binary_to_decimal(&vec![0, 0, 0, 0]), 0);
    assert_eq!(binary_to_decimal(&vec![1]), 1);
    assert_eq!(binary_to_decimal(&vec![0, 0, 0, 1]), 1);
    assert_eq!(binary_to_decimal(&vec![0, 0, 1, 0]), 2);
    assert_eq!(binary_to_decimal(&vec![0, 0, 1, 1]), 3);
    assert_eq!(binary_to_decimal(&vec![1, 0, 0, 0]), 8);
}
