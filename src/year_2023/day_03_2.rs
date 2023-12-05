use std::str::FromStr;

crate::aoc!(2023, 3, 2, solve);

pub fn solve(input: &str) -> anyhow::Result<usize> {
    let grid: Grid = input.parse()?;
    let numbers: Vec<GridNumber> = grid.numbers();
    let stars: Vec<(usize, usize)> = grid.stars();
    let solution = stars
        .into_iter()
        .map(|star| {
            numbers
                .iter()
                .filter(|number| number.is_adjacent(star))
                .collect::<Vec<_>>()
        })
        .filter_map(|numbers| match &numbers[..] {
            &[l, r] => Some(l.number * r.number),
            _ => None,
        })
        .sum();
    Ok(solution)
}

struct Grid(Vec<Vec<char>>);

impl FromStr for Grid {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid = s
            .lines()
            .map(|line| line.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();

        anyhow::ensure!(!grid.is_empty(), "Grid needs 1 or more lines");

        anyhow::ensure!(
            grid.iter().all(|line| !line.is_empty()),
            "Line needs 1 or more characters"
        );

        anyhow::ensure!(
            grid.iter().all(|line| line.len() == grid[0].len()),
            "All lines must be the same length"
        );

        Ok(Self(grid))
    }
}

impl Grid {
    fn numbers(&self) -> Vec<GridNumber> {
        let mut numbers = Vec::new();

        let mut number_digits = String::new();
        let mut number_x = 0;
        let mut number_y = 0;

        for (y, line) in self.0.iter().enumerate() {
            for (x, char) in line.iter().enumerate() {
                if char.is_ascii_digit() {
                    if number_digits.is_empty() {
                        number_x = x;
                        number_y = y;
                    }
                    number_digits.push(*char);
                } else if !number_digits.is_empty() {
                    numbers.push(GridNumber {
                        number: number_digits.parse().unwrap(),
                        x: number_x,
                        y: number_y,
                        length: number_digits.len(),
                    });
                    number_digits.clear();
                }
            }
        }

        numbers
    }

    fn stars(&self) -> Vec<(usize, usize)> {
        let mut stars = Vec::new();

        for (y, line) in self.0.iter().enumerate() {
            for (x, char) in line.iter().enumerate() {
                if *char == '*' {
                    stars.push((x, y));
                }
            }
        }

        stars
    }
}

struct GridNumber {
    number: usize,
    x: usize,
    y: usize,
    length: usize,
}

impl GridNumber {
    fn is_overlapping(&self, (x, y): (usize, usize)) -> bool {
        let min_x = self.x;
        let max_x = self.x + (self.length - 1);
        y == self.y && (min_x..=max_x).contains(&x)
    }

    fn is_adjacent(&self, (x, y): (usize, usize)) -> bool {
        let min_x = self.x.saturating_sub(1);
        let max_x = self.x + (self.length - 1) + 1;
        let min_y = self.y.saturating_sub(1);
        let max_y = self.y + 1;
        (min_x..=max_x).contains(&x) && (min_y..=max_y).contains(&y) && !self.is_overlapping((x, y))
    }
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
        assert_eq!(solve(SAMPLE).unwrap(), 467_835);
    }

    #[test]
    fn test_grid_fromstr() {
        assert!(SAMPLE.parse::<Grid>().is_ok());

        assert_eq!(
            "123\n456\n789\n".parse::<Grid>().unwrap().0,
            vec![
                vec!['1', '2', '3'],
                vec!['4', '5', '6'],
                vec!['7', '8', '9']
            ]
        );

        assert_eq!(
            "123\n456\n789".parse::<Grid>().unwrap().0,
            vec![
                vec!['1', '2', '3'],
                vec!['4', '5', '6'],
                vec!['7', '8', '9']
            ]
        );
    }

    #[test]
    fn test_grid_numbers() {
        let grid = SAMPLE.parse::<Grid>().unwrap();
        let numbers = grid.numbers();

        assert_eq!(
            numbers
                .iter()
                .map(|grid_number| grid_number.number)
                .collect::<Vec<_>>(),
            vec![467, 114, 35, 633, 617, 58, 592, 755, 664, 598],
        );

        assert_eq!(
            numbers
                .iter()
                .map(|grid_number| (grid_number.x, grid_number.y))
                .take(3)
                .collect::<Vec<_>>(),
            vec![(0, 0), (5, 0), (2, 2)],
        );
    }

    #[test]
    fn test_grid_stars() {
        let grid = SAMPLE.parse::<Grid>().unwrap();
        assert_eq!(grid.stars(), vec![(3, 1), (3, 4), (5, 8)]);
    }

    #[test]
    fn test_grid_number_is_overlapping() {
        let grid_number = GridNumber {
            number: 42,
            x: 10,
            y: 10,
            length: 2,
        };
        assert!(!grid_number.is_overlapping((9, 10)));
        assert!(grid_number.is_overlapping((10, 10)));
        assert!(grid_number.is_overlapping((11, 10)));
        assert!(!grid_number.is_overlapping((12, 10)));
        assert!(!grid_number.is_overlapping((10, 11)));
    }

    #[test]
    fn test_grid_number_is_adjacent() {
        let grid_number = GridNumber {
            number: 42,
            x: 10,
            y: 10,
            length: 2,
        };
        assert!(grid_number.is_adjacent((9, 9)));
        assert!(grid_number.is_adjacent((9, 10)));
        assert!(!grid_number.is_adjacent((10, 10)));
        assert!(!grid_number.is_adjacent((11, 10)));
        assert!(grid_number.is_adjacent((12, 10)));
        assert!(grid_number.is_adjacent((10, 11)));
        assert!(grid_number.is_adjacent((12, 11)));
    }
}
