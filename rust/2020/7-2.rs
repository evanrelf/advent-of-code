use once_cell::sync::OnceCell;
use rayon::prelude::*;
use regex::Regex;
use std::collections::HashMap;
use std::io::{self, Read};
use std::str::FromStr;

static BAGS: OnceCell<Vec<Bag>> = OnceCell::new();

static BAG_COLOR_REGEX: OnceCell<Regex> = OnceCell::new();

static BAG_CONTENTS_REGEX: OnceCell<Regex> = OnceCell::new();

#[derive(Clone)]
struct Bag {
    color: String,
    contents: HashMap<String, usize>,
}

impl FromStr for Bag {
    type Err = String;

    fn from_str(string: &str) -> Result<Bag, String> {
        let color_regex = BAG_COLOR_REGEX
            .get_or_init(|| Regex::new(r"(?P<color>\w+(?:\s\w+)*) bags contain").unwrap());

        let contents_regex = BAG_CONTENTS_REGEX.get_or_init(|| {
            Regex::new(r"(?:(?P<count>\d+) (?P<color>\w+(?:\s\w+)*) bags?)").unwrap()
        });

        let color = color_regex
            .captures(string)
            .and_then(|captures| captures.name("color"))
            .map(|color| color.as_str().to_string())
            .ok_or("Failed to parse bag color")?;

        let contents = {
            let mut hashmap = HashMap::new();

            for bag in contents_regex.captures_iter(string) {
                let bag_color = (&bag["color"]).to_string();

                let bag_count = (&bag["count"])
                    .parse()
                    .map_err(|_| "Failed to parse count in bag contents")?;

                hashmap.insert(bag_color, bag_count);
            }

            hashmap
        };

        Ok(Bag { color, contents })
    }
}

impl Bag {
    fn count_contents(&self) -> usize {
        if self.contents.is_empty() {
            return 0;
        }

        self.contents
            .par_iter()
            .map(|(color, count)| {
                let bag = lookup_bag(color).expect(&format!("Unexpected bag color: '{}'", color));
                count + (count * bag.count_contents())
            })
            .sum()
    }
}

fn lookup_bag(color: &str) -> Option<&Bag> {
    let bags = BAGS.get().expect("BAGS not initialized");

    bags.iter().find(|bag| bag.color == color)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    let bags: Vec<_> = input.lines().map(|line| line.parse().unwrap()).collect();

    BAGS.set(bags.clone()).ok().unwrap();

    let color = "shiny gold";

    let count = lookup_bag(color)
        .expect(&format!("Missing {} bag", color))
        .count_contents();

    println!("The {} bag contains {} bags", color, count);

    Ok(())
}
