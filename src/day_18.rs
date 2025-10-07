use std::collections::HashMap;
use std::fmt::{Debug, Display, Write};
use std::mem::MaybeUninit;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Invalid tile")]
    InvalidTile,
    #[error("Grid rows are jagged")]
    JaggedGrid,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(C)]
struct Grid<T> {
    tiles: Box<[T]>,
    rows: usize,
    cols: usize,
}

impl<T> Grid<T> {
    fn new(rows: usize, cols: usize, cb: impl Fn(usize, usize) -> T) -> Self {
        let tiles = (0..rows * cols)
            .map(|ix| cb(ix / cols, ix % cols))
            .collect();
        Self { tiles, rows, cols }
    }

    fn to_index(&self, pos: Position) -> Option<usize> {
        (pos.row < self.rows && pos.col < self.cols).then_some(pos.row * self.cols + pos.col)
    }
}

impl<T> Index<Position> for Grid<T> {
    type Output = T;

    fn index(&self, pos: Position) -> &Self::Output {
        self.to_index(pos).map_or_else(
            || {
                panic!("outside: {pos:?}");
            },
            |index| &self.tiles[index],
        )
    }
}

impl<T> IndexMut<Position> for Grid<T> {
    fn index_mut(&mut self, pos: Position) -> &mut Self::Output {
        self.to_index(pos).map_or_else(
            || {
                panic!("outside: {pos:?}");
            },
            |index| &mut self.tiles[index],
        )
    }
}

impl<T> Display for Grid<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in 0..self.rows {
            if row > 0 {
                writeln!(f)?;
            }
            for cell in &self.tiles[row * self.cols..(row + 1) * self.cols] {
                cell.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl<T> FromStr for Grid<T>
where
    T: TryFrom<u8>,
    ParseError: From<T::Error>,
{
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rows = 0;
        let mut cols = 0;
        for line in s.lines() {
            rows += 1;
            cols = line.len();
        }
        let mut grid = Grid::new(rows, cols, |_, _| MaybeUninit::uninit());
        for (row, line) in s.lines().enumerate() {
            if line.len() != cols {
                return Err(ParseError::JaggedGrid);
            }
            for (col, ch) in line.bytes().enumerate() {
                let pos = Position { row, col };
                grid[pos] = MaybeUninit::new(ch.try_into()?);
            }
        }
        // SAFETY: MaybeUninit<T> is guaranteed to be transmutable to T if it does not have a strange memory layout.
        Ok(unsafe { std::mem::transmute::<Grid<MaybeUninit<T>>, Self>(grid) })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Position {
    row: usize,
    col: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("").field(&self.col).field(&self.row).finish()
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("").field(&self.col).field(&self.row).finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Open,
    Trees,
    Lumberyard,
}
impl Tile {
    const fn evolve(self, counts: [u8; 3]) -> Self {
        match (self, counts) {
            (Self::Open, [_, 3..=8, _]) => Self::Trees,
            (Self::Trees, [_, _, 3..=8]) => Self::Lumberyard,
            (Self::Lumberyard, [_, 0, _] | [_, _, 0]) => Self::Open,
            _ => self,
        }
    }
}

impl TryFrom<u8> for Tile {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'.' => Self::Open,
            b'|' => Self::Trees,
            b'#' => Self::Lumberyard,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Open => f.write_char('.'),
            Self::Trees => f.write_char('|'),
            Self::Lumberyard => f.write_char('#'),
        }
    }
}

#[aoc_generator(day18)]
fn parse(input: &str) -> Result<Grid<Tile>, ParseError> {
    input.parse()
}

#[aoc(day18, part1)]
fn part_1(grid: &Grid<Tile>) -> usize {
    simulate(grid, 10)
}

#[aoc(day18, part2)]
fn part_2(grid: &Grid<Tile>) -> usize {
    simulate(grid, 1_000_000_000)
}

fn simulate(grid: &Grid<Tile>, minutes: usize) -> usize {
    let mut current = grid.clone();
    let mut next = grid.clone();
    let mut seen = HashMap::new();
    let mut rotating_key = [[0; 3]; 2];
    let mut time = 0;
    while time < minutes {
        step(&mut next, &current);
        (current, next) = (next, current);

        let mut counts = [0; 3];
        for &tile in &current.tiles {
            counts[tile as usize] += 1;
        }

        rotating_key[0] = counts;
        rotating_key.rotate_left(1);
        if let Some(&past) = seen.get(&rotating_key) {
            let cycle_len = time - past;
            let skip_cycles = (minutes - time) / cycle_len;
            time += skip_cycles * cycle_len;
        }
        seen.insert(rotating_key, time);
        time += 1;
    }

    let mut counts = [0; 3];
    for &tile in &current.tiles {
        counts[tile as usize] += 1;
    }
    counts[Tile::Lumberyard as usize] * counts[Tile::Trees as usize]
}

fn step(next: &mut Grid<Tile>, current: &Grid<Tile>) {
    for row in 0..next.rows {
        for col in 0..next.cols {
            let mut counts = [0; 3];
            for (r, c) in [
                (row > 0 && col > 0).then(|| (row - 1, col - 1)),
                (row > 0).then(|| (row - 1, col)),
                (row > 0 && col + 1 < next.cols).then(|| (row - 1, col + 1)),
                (col > 0).then(|| (row, col - 1)),
                (col + 1 < next.cols).then(|| (row, col + 1)),
                (row + 1 < next.rows && col > 0).then(|| (row + 1, col - 1)),
                (row + 1 < next.rows).then(|| (row + 1, col)),
                (row + 1 < next.rows && col + 1 < next.cols).then(|| (row + 1, col + 1)),
            ]
            .into_iter()
            .flatten()
            {
                counts[current[Position { row: r, col: c }] as usize] += 1;
            }
            let pos = Position { row, col };
            next[pos] = current[pos].evolve(counts);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        .#.#...|#.\n\
        .....#|##|\n\
        .|..|...#.\n\
        ..|#.....#\n\
        #.#|||#|#|\n\
        ...#.||...\n\
        .|....|...\n\
        ||...#|.#|\n\
        |.||||..|.\n\
        ...#.|..|.\
    ";

    #[test]
    fn test_part_1() {
        let grid = parse(EXAMPLE).unwrap();
        let result = simulate(&grid, 10);
        assert_eq!(result, 1147);
    }
}
