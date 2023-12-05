use std::{collections::HashSet, convert::Infallible, str::FromStr};

crate::aoc!(2023, 3, 1, solve);

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let grid = input.parse::<Grid>()?;
    let numbers = grid.numbers()?;
    let part_numbers = numbers
        .into_iter()
        .filter(|gn| gn.is_part_number(&grid))
        .map(|gn| gn.number)
        .collect::<Vec<_>>();
    let solution = part_numbers.into_iter().sum();
    Ok(solution)
}

#[derive(Debug, PartialEq)]
struct Grid(Vec<Vec<char>>);

impl FromStr for Grid {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.lines().map(|line| line.chars().collect()).collect()))
    }
}

impl Grid {
    fn numbers(&self) -> anyhow::Result<Vec<GridNumber>> {
        let Self(grid) = self;

        let height = grid.len();
        let width = grid[0].len();

        let mut numbers = Vec::new();

        let mut number_string = String::new();
        let mut start_x = 0;
        let mut start_y = 0;

        #[allow(clippy::needless_range_loop)]
        for y in 0..height {
            for x in 0..width {
                let char = grid[y][x];

                if char.is_ascii_digit() {
                    if number_string.is_empty() {
                        start_x = x;
                        start_y = y;
                    }
                    number_string.push(char);
                } else if !number_string.is_empty() {
                    let number = number_string.parse()?;
                    number_string.clear();
                    numbers.push(GridNumber {
                        number,
                        start_x,
                        start_y,
                    });
                }
            }
        }

        Ok(numbers)
    }
}

#[derive(Debug, PartialEq)]
struct GridNumber {
    number: usize,
    start_x: usize,
    start_y: usize,
}

impl GridNumber {
    fn length(&self) -> usize {
        self.number.to_string().len()
    }

    #[rustfmt::skip]
    #[allow(clippy::many_single_char_names)]
    fn adjacencies(&self, Grid(grid): &Grid) -> HashSet<(usize, usize)> {
        let max_y = grid.len() - 1;
        let max_x = grid[0].len() - 1;

        let mut coordinates = HashSet::new();

        let y = self.start_y;
        let start_x = self.start_x;
        let end_x = start_x + (self.length() - 1);

        for x in start_x..=end_x {
            coordinates.extend(adjacencies(x, y, max_x, max_y));
        }

        coordinates
    }

    fn is_part_number(&self, grid: &Grid) -> bool {
        self.adjacencies(grid)
            .into_iter()
            .map(|(x, y)| grid.0[y][x])
            .any(|c| c != '.' && !c.is_ascii_digit())
    }
}

#[rustfmt::skip]
#[allow(clippy::many_single_char_names)]
fn adjacencies(x: usize, y: usize, max_x: usize, max_y: usize) -> HashSet<(usize, usize)> {
    let mut coordinates = HashSet::new();

    let top = y == 0;
    let bottom = y >= max_y;
    let left = x == 0;
    let right = x >= max_x;

    let n = !top;
    let s = !bottom;
    let e = !right;
    let w = !left;
    let ne = n && e;
    let nw = n && w;
    let se = s && e;
    let sw = s && w;

    if n { coordinates.insert((x, y - 1)); }
    if s { coordinates.insert((x, y + 1)); }
    if e { coordinates.insert((x + 1, y)); }
    if w { coordinates.insert((x - 1, y)); }
    if ne { coordinates.insert((x + 1, y - 1)); }
    if nw { coordinates.insert((x - 1, y - 1)); }
    if se { coordinates.insert((x + 1, y + 1)); }
    if sw { coordinates.insert((x - 1, y + 1)); }

    coordinates
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        467..114..
        ...*......
        ..35..633.
        ......#...
        617*......
        .....+.58.
        ..592.....
        ......755.
        ...$.*....
        .664.598..
    "};

    #[test]
    fn test_sample() {
        assert_eq!(solve(SAMPLE).unwrap(), 4361);
    }

    #[test]
    fn test_grid_fromstr() {
        let input = indoc! {"
            123
            456
            789
        "};
        let grid = Grid(vec![
            vec!['1', '2', '3'],
            vec!['4', '5', '6'],
            vec!['7', '8', '9'],
        ]);
        assert_eq!(input.parse::<Grid>().unwrap(), grid);
    }

    #[test]
    fn test_grid_numbers() {
        let grid = SAMPLE.parse::<Grid>().unwrap();
        let grid_numbers = grid.numbers().unwrap();
        assert_eq!(
            grid_numbers.iter().filter(|gn| gn.number == 598).last(),
            Some(&GridNumber {
                number: 598,
                start_x: 5,
                start_y: 9
            })
        );
        let numbers = grid_numbers
            .into_iter()
            .map(|gn| gn.number)
            .collect::<Vec<_>>();
        assert_eq!(
            numbers,
            vec![467, 114, 35, 633, 617, 58, 592, 755, 664, 598]
        );
    }

    #[test]
    fn test_adjacencies() {
        let coordinate = (100, 100);
        let expected = {
            let mut coordinates = HashSet::new();
            for x in 99..=101 {
                for y in 99..=101 {
                    if (x, y) != coordinate {
                        coordinates.insert((x, y));
                    }
                }
            }
            coordinates
        };
        let actual = adjacencies(100, 100, usize::MAX, usize::MAX);
        assert_eq!(expected, actual);

        let coordinate = (0, 0);
        let expected = {
            let mut coordinates = HashSet::new();
            for x in 0..=1 {
                for y in 0..=1 {
                    if (x, y) != coordinate {
                        coordinates.insert((x, y));
                    }
                }
            }
            coordinates
        };
        let actual = adjacencies(0, 0, usize::MAX, usize::MAX);
        assert_eq!(expected, actual);

        let expected = HashSet::new();
        let actual = adjacencies(0, 0, 0, 0);
        assert_eq!(expected, actual);
    }
}
