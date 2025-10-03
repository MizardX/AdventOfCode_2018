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
struct Claim {
    id: usize,
    left: usize,
    top: usize,
    width: usize,
    height: usize,
}

impl Claim {
    const fn right(&self) -> usize {
        self.left + self.width
    }

    const fn bottom(&self) -> usize {
        self.top + self.height
    }

    const fn overlaps(&self, other: &Self) -> bool {
        self.right() > other.left
            && self.left < other.right()
            && self.bottom() > other.top
            && self.top < other.bottom()
    }
}

impl FromStr for Claim {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rest = s.strip_prefix('#').ok_or(ParseError::SyntaxError)?;
        let (id, rest) = rest.split_once(" @ ").ok_or(ParseError::SyntaxError)?;
        let (left, rest) = rest.split_once(',').ok_or(ParseError::SyntaxError)?;
        let (top, rest) = rest.split_once(": ").ok_or(ParseError::SyntaxError)?;
        let (width, height) = rest.split_once('x').ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            id: id.parse()?,
            left: left.parse()?,
            top: top.parse()?,
            width: width.parse()?,
            height: height.parse()?,
        })
    }
}

#[aoc_generator(day3)]
fn parse(input: &str) -> Result<Vec<Claim>, ParseError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day3, part1)]
fn part_1(claims: &[Claim]) -> usize {
    const STRIDE: usize = 1000;
    let mut grid = vec![0_u8; STRIDE * STRIDE];
    for claim in claims {
        for y in claim.top..claim.bottom() {
            for cnt in &mut grid[STRIDE * y + claim.left..STRIDE * y + claim.right()] {
                *cnt += 1;
            }
        }
    }
    grid.into_iter().filter(|&x| x > 1).count()
}

#[aoc(day3, part2)]
fn part_2(claims: &[Claim]) -> usize {
    'next_candidate: for candidate in claims {
        for other in claims {
            if other.id != candidate.id && candidate.overlaps(other) {
                continue 'next_candidate;
            }
        }
        return candidate.id;
    }
    usize::MAX
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        #1 @ 1,3: 4x4\n\
        #2 @ 3,1: 4x4\n\
        #3 @ 5,5: 2x2\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            [
                Claim {
                    id: 1,
                    left: 1,
                    top: 3,
                    width: 4,
                    height: 4
                },
                Claim {
                    id: 2,
                    left: 3,
                    top: 1,
                    width: 4,
                    height: 4
                },
                Claim {
                    id: 3,
                    left: 5,
                    top: 5,
                    width: 2,
                    height: 2
                }
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let claims = parse(EXAMPLE).unwrap();
        let result = part_1(&claims);
        assert_eq!(result, 4);
    }

    #[test]
    fn test_part_2() {
        let claims = parse(EXAMPLE).unwrap();
        let result = part_2(&claims);
        assert_eq!(result, 3);
    }
}
