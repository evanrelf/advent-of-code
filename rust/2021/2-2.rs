use std::{io::Read, str::FromStr};

enum Command {
    Forward(u32),
    Down(u32),
    Up(u32),
}

impl FromStr for Command {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let words = string.split(' ').collect::<Vec<_>>();

        let direction = words.get(0).ok_or("Missing direction")?;

        let distance = {
            let distance_string = words.get(1).ok_or("Missing distance")?;
            distance_string
                .parse()
                .map_err(|_| format!("Failed to parse distance from '{}'", distance_string))?
        };

        match direction {
            &"forward" => Ok(Command::Forward(distance)),
            &"down" => Ok(Command::Down(distance)),
            &"up" => Ok(Command::Up(distance)),
            invalid => Err(format!("Invalid direction '{}'", invalid)),
        }
    }
}

struct Submarine {
    horizontal_position: i32,
    depth: i32,
    aim: i32,
}

impl Submarine {
    fn execute(&mut self, command: &Command) {
        match command {
            Command::Forward(distance) => {
                self.horizontal_position += *distance as i32;
                self.depth += self.aim * *distance as i32;
            }
            Command::Down(distance) => self.aim += *distance as i32,
            Command::Up(distance) => self.aim -= *distance as i32,
        }
    }
}

impl Default for Submarine {
    fn default() -> Submarine {
        Submarine {
            horizontal_position: 0,
            depth: 0,
            aim: 0,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = std::io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let commands = input
        .lines()
        .map(|command| command.parse().unwrap())
        .collect::<Vec<_>>();

    let mut submarine = Submarine::default();

    for command in commands {
        submarine.execute(&command);
    }

    let solution = submarine.horizontal_position * submarine.depth;

    println!("{}", solution);

    Ok(())
}
