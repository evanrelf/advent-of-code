use once_cell::sync::OnceCell;
use rayon::prelude::*;
use regex::Regex;
use std::collections::HashMap;
use std::io::{self, Read};
use std::str::FromStr;

// Global variable storing all bags from problem input
static BAGS: OnceCell<Vec<Bag>> = OnceCell::new();

// Global variables storing regexes, so we don't recompile them over and over
static BAG_COLOR_REGEX: OnceCell<Regex> = OnceCell::new();
static BAG_CONTENTS_REGEX: OnceCell<Regex> = OnceCell::new();

#[derive(Clone)]
struct Bag {
    // > light red bags contain 1 bright white bag, 2 muted yellow bags.
    //   ^^^^^^^^^
    color: String,
    // > light red bags contain 1 bright white bag, 2 muted yellow bags.
    //                          ^^^^^^^^^^^^^^      ^^^^^^^^^^^^^^
    contents: HashMap<String, u16>,
}

// The `FromStr` trait in Rust is what the `parse` method uses under-the-hood to convert strings to
// other data types.
//
// It's already implemented for types in the standard library like `bool` or `u8`, but we need to
// implement it for our custom `Bag` type so we can parse it.
impl FromStr for Bag {
    type Err = String;

    // Given a string, you _might_ get a bag back, or you'll get an error message. That's what
    // `Result<Bag, String>` means.
    fn from_str(string: &str) -> Result<Bag, String> {
        // If this is the first time using these regexes, we need to compile them and store them for
        // future use. If it's not the first time, we just retrieve the previously compiled
        // versions.
        let color_regex = BAG_COLOR_REGEX
            .get_or_init(|| Regex::new(r"(?P<color>\w+(?:\s\w+)*) bags contain").unwrap());

        let contents_regex = BAG_CONTENTS_REGEX.get_or_init(|| {
            Regex::new(r"(?:(?P<count>\d+) (?P<color>\w+(?:\s\w+)*) bags?)").unwrap()
        });

        let color = color_regex
            // Give me the capture groups you got from matching the regex
            .captures(string)
            // I want the first (and only) one which I named "color"
            .and_then(|captures| captures.name("color"))
            // I don't care about it's position in the overall string, I just want the matching
            // string
            .map(|color| color.as_str().to_string())
            // If any of the previous steps failed, let's use this as the error message
            .ok_or("Failed to parse bag color")?;

        let contents = {
            // Start with an empty `HashMap` representing empty bag contents
            let mut hashmap = HashMap::new();

            // Loop through all the capture groups from matching the regex to find the contained
            // bags.
            //
            // If the regex fails to match (for example, "no other bags" doesn't actually match my
            // regex), there just won't be anything to loop through. "Failure to match the regex"
            // can be the same as "bag is empty" as far as I'm concerned, considering this is a
            // small program.
            for bag in contents_regex.captures_iter(string) {
                // Give me the capture group I named "color"
                let bag_color = (&bag["color"]).to_string();

                // Give me the capture group I named "count"
                let bag_count = (&bag["count"])
                    // ...and parse the string into a number
                    .parse()
                    // ...and use this error message if you fail
                    .map_err(|_| "Failed to parse count in bag contents")?;

                // Alright, now add it to our `HashMap`, which will become our bag contents
                hashmap.insert(bag_color, bag_count);
            }

            hashmap
        };

        // If we've gotten to this point, then we have parsed `color` and `contents` successfully,
        // so we can construct a `Bag` and return it.
        Ok(Bag { color, contents })
    }
}

impl Bag {
    // Determine whether a bag can contain another bag
    fn can_contain(&self, search_color: &str) -> bool {
        // Base case of recursion (we treat bags as containing themselves at this stage)
        if self.color == search_color {
            return true;
        }

        // Search through all the bags contained in this bag, checking whether they have the shiny
        // gold bag (in parallel).
        self.contents
            .par_iter()
            // `find_any` searches until one bag has it, and then quits. No point searching through
            // 900 more bags if you found it on bag #100.
            .find_any(|(color, _)| {
                let bag = lookup_bag(color).expect(&format!("Unexpected bag color: '{}'", color));
                bag.can_contain(search_color)
            })
            .is_some()
    }
}

// Given a color, lookup the corresponding bag (so we can inspect its contents)
fn lookup_bag(color: &str) -> Option<&Bag> {
    // Read global variable containing all bags
    let bags = BAGS.get().expect("BAGS not initialized");

    // Search for the bag with the specified color. This assumes only one will be
    // present in the vector, which is true for the input provided by Advent of Code.
    bags.iter().find(|bag| bag.color == color)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read problem input piped in from `stdin`
    let input = {
        let mut buffer = String::new();
        let mut stdin = io::stdin();
        stdin.read_to_string(&mut buffer)?;
        buffer
    };

    // Parse each line of input into a bag
    let bags: Vec<_> = input.lines().map(|line| line.parse().unwrap()).collect();

    // Populate `BAGS` global variable (for convenience, so we don't have to pass it around)
    BAGS.set(bags.clone()).ok().unwrap();

    let color = "shiny gold";

    // Check each bag to see if it contains the shiny gold bag (in parallel), and count how many do
    let count = bags
        .par_iter()
        // We don't include the bag we're searching for in the count, because a bag can't contain
        // itself (does the same thing as subtracting one from the final count, but makes more sense
        // to me)
        .filter(|bag| bag.color != color && bag.can_contain(color))
        .count();

    println!("{} bag(s) contain the {} bag", count, color);

    Ok(())
}
