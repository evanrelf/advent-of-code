use anyhow::anyhow;
use derive_more::{From, Into};
use std::str::FromStr;
use winnow::{
    ascii::{digit1, line_ending},
    combinator::{delimited, repeat, separated, terminated},
    prelude::*,
};

crate::aoc!();

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let almanac = input.parse::<Almanac>()?;
    let solution = almanac
        .seeds
        .iter()
        .map(|seed| almanac.seed_to_location(*seed).0)
        .min()
        .unwrap();
    Ok(solution)
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<Seed>,
    seed_to_soil: Map<Seed, Soil>,
    soil_to_fertilizer: Map<Soil, Fertilizer>,
    fertilizer_to_water: Map<Fertilizer, Water>,
    water_to_light: Map<Water, Light>,
    light_to_temperature: Map<Light, Temperature>,
    temperature_to_humidity: Map<Temperature, Humidity>,
    humidity_to_location: Map<Humidity, Location>,
}

impl Almanac {
    fn seed_to_location(&self, seed: Seed) -> Location {
        let soil = self.seed_to_soil.map(&seed);
        let fertilizer = self.soil_to_fertilizer.map(&soil);
        let water = self.fertilizer_to_water.map(&fertilizer);
        let light = self.water_to_light.map(&water);
        let temperature = self.light_to_temperature.map(&light);
        let humidity = self.temperature_to_humidity.map(&temperature);
        self.humidity_to_location.map(&humidity)
    }
}

impl FromStr for Almanac {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        almanac.parse(s).map_err(|e| anyhow!(e.to_string()))
    }
}

fn almanac(input: &mut &str) -> PResult<Almanac> {
    fn line_endings0(input: &mut &str) -> PResult<()> {
        repeat(0.., line_ending).parse_next(input)
    }

    fn line_endings2(input: &mut &str) -> PResult<()> {
        repeat(2, line_ending).parse_next(input)
    }

    let seeds = delimited(
        "seeds: ",
        separated(1.., usize.map(Seed), " "),
        line_endings2,
    )
    .parse_next(input)?;
    let seed_to_soil = terminated(map::<Seed, Soil>, line_endings2).parse_next(input)?;
    let soil_to_fertilizer =
        terminated(map::<Soil, Fertilizer>, line_endings2).parse_next(input)?;
    let fertilizer_to_water =
        terminated(map::<Fertilizer, Water>, line_endings2).parse_next(input)?;
    let water_to_light = terminated(map::<Water, Light>, line_endings2).parse_next(input)?;
    let light_to_temperature =
        terminated(map::<Light, Temperature>, line_endings2).parse_next(input)?;
    let temperature_to_humidity =
        terminated(map::<Temperature, Humidity>, line_endings2).parse_next(input)?;
    let humidity_to_location =
        terminated(map::<Humidity, Location>, line_endings0).parse_next(input)?;

    Ok(Almanac {
        seeds,
        seed_to_soil,
        soil_to_fertilizer,
        fertilizer_to_water,
        water_to_light,
        light_to_temperature,
        temperature_to_humidity,
        humidity_to_location,
    })
}

#[derive(Debug)]
struct Map<S, D> {
    ranges: Vec<Range<S, D>>,
}

impl<S, D> Map<S, D>
where
    S: Category,
    D: Category,
{
    fn map(&self, source: &S) -> D {
        self.ranges
            .iter()
            .find_map(|range| range.map(source))
            .unwrap_or_else(|| D::from((*source).into()))
    }
}

fn map<S, D>(input: &mut &str) -> PResult<Map<S, D>>
where
    S: Category,
    D: Category,
{
    let _ = (S::name(), "-to-", D::name(), " map:\n").parse_next(input)?;
    let ranges = separated(1.., range, line_ending).parse_next(input)?;
    Ok(Map { ranges })
}

#[derive(Debug)]
struct Range<S, D> {
    destination_range_start: D,
    source_range_start: S,
    #[allow(clippy::struct_field_names)]
    range_length: usize,
}

impl<S, D> Range<S, D>
where
    S: Category,
    D: Category,
{
    fn map(&self, source: &S) -> Option<D> {
        let source = (*source).into();
        let source_range_start = self.source_range_start.into();
        let destination_range_start = self.destination_range_start.into();

        let source_range = source_range_start..(source_range_start + self.range_length);

        if !source_range.contains(&source) {
            return None;
        }

        let offset = source - source_range_start;

        let destination = destination_range_start + offset;

        Some(D::from(destination))
    }
}

fn range<S, D>(input: &mut &str) -> PResult<Range<S, D>>
where
    S: Category,
    D: Category,
{
    let destination_range_start = terminated(usize.map(D::from), " ").parse_next(input)?;
    let source_range_start = terminated(usize.map(S::from), " ").parse_next(input)?;
    let range_length = usize.parse_next(input)?;

    Ok(Range {
        destination_range_start,
        source_range_start,
        range_length,
    })
}

fn usize(input: &mut &str) -> PResult<usize> {
    digit1.try_map(|s: &str| s.parse()).parse_next(input)
}

trait Category: Clone + Copy + From<usize> + Into<usize> + Name {}

impl<T> Category for T where T: Clone + Copy + From<usize> + Into<usize> + Name {}

trait Name {
    fn name() -> &'static str;
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Seed(usize);

impl Name for Seed {
    fn name() -> &'static str {
        "seed"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Soil(usize);

impl Name for Soil {
    fn name() -> &'static str {
        "soil"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Fertilizer(usize);

impl Name for Fertilizer {
    fn name() -> &'static str {
        "fertilizer"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Water(usize);

impl Name for Water {
    fn name() -> &'static str {
        "water"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Light(usize);

impl Name for Light {
    fn name() -> &'static str {
        "light"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Temperature(usize);

impl Name for Temperature {
    fn name() -> &'static str {
        "temperature"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Humidity(usize);

impl Name for Humidity {
    fn name() -> &'static str {
        "humidity"
    }
}

#[derive(Clone, Copy, Debug, From, Into)]
struct Location(usize);

impl Name for Location {
    fn name() -> &'static str {
        "location"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        soil-to-fertilizer map:
        0 15 37
        37 52 2
        39 0 15

        fertilizer-to-water map:
        49 53 8
        0 11 42
        42 0 7
        57 7 4

        water-to-light map:
        88 18 7
        18 25 70

        light-to-temperature map:
        45 77 23
        81 45 19
        68 64 13

        temperature-to-humidity map:
        0 69 1
        1 0 69

        humidity-to-location map:
        60 56 37
        56 93 4
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 35);
    }
}
