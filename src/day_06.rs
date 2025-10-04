use std::num::ParseIntError;
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Syntax error")]
    SyntaxError,
    #[error(transparent)]
    InvalidNumber(#[from] ParseIntError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coordinate(u32, u32);

impl Coordinate {
    const fn dist(self, x: u32, y: u32) -> u32 {
        self.0.abs_diff(x) + self.1.abs_diff(y)
    }
}

impl FromStr for Coordinate {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s.split_once(", ").ok_or(ParseError::SyntaxError)?;
        Ok(Self(x.parse()?, y.parse()?))
    }
}

#[aoc_generator(day6)]
fn parse(input: &str) -> Result<Vec<Coordinate>, ParseError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day6, part1)]
fn part_1(coordinates: &[Coordinate]) -> usize {
    find_largest_finite_region(coordinates, 1000)
}

fn find_largest_finite_region(coordinates: &[Coordinate], grid_size: u32) -> usize {
    let mut sizes = vec![0; coordinates.len()];
    let mut infinite = vec![false; coordinates.len()];
    for y in 0..grid_size {
        for x in 0..grid_size {
            let mut minimum = u32::MAX;
            let mut min_index = 0;
            let mut count = 0;
            for (index, &coord) in coordinates.iter().enumerate() {
                let dist = coord.dist(x, y);
                if dist < minimum {
                    minimum = dist;
                    min_index = index;
                    count = 1;
                } else if dist == minimum {
                    count += 1;
                }
            }
            if count == 1 {
                if x == 0 || x == grid_size - 1 || y == 0 || y == grid_size - 1 {
                    infinite[min_index] = true;
                }
                sizes[min_index] += 1;
            }
        }
    }
    sizes
        .into_iter()
        .zip(infinite)
        .filter_map(|(s, inf)| (!inf).then_some(s))
        .max()
        .unwrap()
}

#[aoc(day6, part2)]
fn part_2(coordinates: &[Coordinate]) -> usize {
    measure_close_region(coordinates, 1000, 1000)
}

fn measure_close_region(coordinates: &[Coordinate], grid_size: u32, dist_limit: u32) -> usize {
    let mut region_size = 0;
    for y in 0..grid_size {
        for x in 0..grid_size {
            let total_dist: u32 = coordinates.iter().map(|c| c.dist(x, y)).sum();
            if total_dist < dist_limit {
                region_size += 1;
            }
        }
    }
    region_size
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        1, 1\n\
        1, 6\n\
        8, 3\n\
        3, 4\n\
        5, 5\n\
        8, 9\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            [
                Coordinate(1, 1),
                Coordinate(1, 6),
                Coordinate(8, 3),
                Coordinate(3, 4),
                Coordinate(5, 5),
                Coordinate(8, 9),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let coordinates = parse(EXAMPLE).unwrap();
        let result = find_largest_finite_region(&coordinates, 10);
        assert_eq!(result, 17);
    }

    #[test]
    fn test_part_2() {
        let coordinates = parse(EXAMPLE).unwrap();
        let result = measure_close_region(&coordinates, 10, 32);
        assert_eq!(result, 16);
    }
}
