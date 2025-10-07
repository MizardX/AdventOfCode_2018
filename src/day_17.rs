use std::fmt::{Debug, Display, Write};
use std::num::ParseIntError;
use std::ops::{Add, Index, IndexMut};
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
enum Clay {
    Vertical(Vertical),
    Horizontal(Horizontal),
}

impl FromStr for Clay {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match &s[0..2] {
            "x=" => Self::Vertical(s.parse()?),
            "y=" => Self::Horizontal(s.parse()?),
            _ => return Err(ParseError::SyntaxError),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Vertical {
    x: i32,
    y1: i32,
    y2: i32,
}

impl FromStr for Vertical {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, rest) = s
            .strip_prefix("x=")
            .ok_or(ParseError::SyntaxError)?
            .split_once(", y=")
            .ok_or(ParseError::SyntaxError)?;
        let (y1, y2) = rest.split_once("..").ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            x: x.parse()?,
            y1: y1.parse()?,
            y2: y2.parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Horizontal {
    y: i32,
    x1: i32,
    x2: i32,
}

impl FromStr for Horizontal {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (y, rest) = s
            .strip_prefix("y=")
            .ok_or(ParseError::SyntaxError)?
            .split_once(", x=")
            .ok_or(ParseError::SyntaxError)?;
        let (x1, x2) = rest.split_once("..").ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            y: y.parse()?,
            x1: x1.parse()?,
            x2: x2.parse()?,
        })
    }
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

    fn contains(&self, pos: Position) -> bool {
        self.to_index(pos).is_some()
    }

    fn to_index(&self, pos: Position) -> Option<usize> {
        let row = usize::try_from(pos.row).ok()?;
        let col = usize::try_from(pos.col).ok()?;
        (row < self.rows && col < self.cols).then_some(row * self.cols + col)
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Position {
    row: i32,
    col: i32,
}

#[allow(unused)]
impl Position {
    const fn with_col(self, col: i32) -> Self {
        Self { col, ..self }
    }
    const fn with_row(self, row: i32) -> Self {
        Self { row, ..self }
    }
}

impl Add<Direction> for Position {
    type Output = Self;

    fn add(mut self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::Up => self.row -= 1,
            Direction::Right => self.col += 1,
            Direction::Down => self.row += 1,
            Direction::Left => self.col -= 1,
        }
        self
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
#[repr(u8)]
enum Direction {
    #[default]
    Up,
    Left,
    Right,
    Down,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Sand,
    Clay,
    TrappedWater,
    FlowingWater,
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sand => f.write_char('.'),
            Self::Clay => f.write_char('#'),
            Self::TrappedWater => f.write_char('~'),
            Self::FlowingWater => f.write_char('|'),
        }
    }
}

#[aoc_generator(day17)]
fn parse(input: &str) -> Result<Vec<Clay>, ParseError> {
    input.lines().map(str::parse).collect()
}

fn create_grid(clay: &[Clay]) -> (Grid<Tile>, Position) {
    let mut min_x = i32::MAX;
    let mut max_x = i32::MIN;
    let mut min_y = i32::MAX;
    let mut max_y = i32::MIN;
    for &line in clay {
        match line {
            Clay::Vertical(Vertical { x, y1, y2 }) => {
                min_x = min_x.min(x);
                max_x = max_x.max(x);
                min_y = min_y.min(y1);
                max_y = max_y.max(y2);
            }
            Clay::Horizontal(Horizontal { y, x1, x2 }) => {
                min_x = min_x.min(x1);
                max_x = max_x.max(x2);
                min_y = min_y.min(y);
                max_y = max_y.max(y);
            }
        }
    }
    // Give 1 tile margin left and right, to allow stream to flow down there.
    min_x -= 1;
    max_x += 1;
    let mut grid = Grid::new(
        usize::try_from(max_y + 1 - min_y).unwrap(),
        usize::try_from(max_x + 1 - min_x).unwrap(),
        |_, _| Tile::Sand,
    );
    for &line in clay {
        match line {
            Clay::Vertical(Vertical { x, y1, y2 }) => {
                for y in y1..=y2 {
                    grid[Position {
                        row: y - min_y,
                        col: x - min_x,
                    }] = Tile::Clay;
                }
            }
            Clay::Horizontal(Horizontal { y, x1, x2 }) => {
                for x in x1..=x2 {
                    grid[Position {
                        row: y - min_y,
                        col: x - min_x,
                    }] = Tile::Clay;
                }
            }
        }
    }
    let initial_source = Position {
        row: 0,
        col: 500 - min_x,
    };
    (grid, initial_source)
}

fn simulate_water(grid: &mut Grid<Tile>, initial_source: Position) {
    grid[initial_source] = Tile::FlowingWater;

    let mut pending = Vec::new();
    pending.push(initial_source);
    'pending: while let Some(mut pos) = pending.pop() {
        let mut below = pos + Direction::Down;
        if !grid.contains(below) {
            continue;
        }
        while matches!(grid[below], Tile::FlowingWater | Tile::Sand) {
            pos = below;
            grid[pos] = Tile::FlowingWater;
            below = pos + Direction::Down;
            if !grid.contains(below) {
                continue 'pending;
            }
        }
        let (left_bound, right_bound) = find_flow_bounds(grid, pos);
        if let FlowBound::Spill(left) = left_bound
            && grid[left] != Tile::FlowingWater
        {
            pending.push(left);
        }
        if let FlowBound::Spill(right) = right_bound
            && grid[right] != Tile::FlowingWater
        {
            pending.push(right);
        }
        match (left_bound, right_bound) {
            (FlowBound::Wall(left), FlowBound::Wall(right)) => {
                for col in left.col..=right.col {
                    grid[pos.with_col(col)] = Tile::TrappedWater;
                }
                pending.push(pos + Direction::Up);
            }
            (
                FlowBound::Wall(left) | FlowBound::Spill(left) | FlowBound::Outside(left),
                FlowBound::Wall(right) | FlowBound::Spill(right) | FlowBound::Outside(right),
            ) => {
                for col in left.col..=right.col {
                    grid[pos.with_col(col)] = Tile::FlowingWater;
                }
            }
        }
    }
}

#[aoc(day17, part1)]
fn part_1(clay: &[Clay]) -> usize {
    let (mut grid, initial_source) = create_grid(clay);

    simulate_water(&mut grid, initial_source);

    grid.tiles
        .iter()
        .filter(|&tile| matches!(tile, Tile::FlowingWater | Tile::TrappedWater))
        .count()
}

#[aoc(day17, part2)]
fn part_2(clay: &[Clay]) -> usize {
    let (mut grid, initial_source) = create_grid(clay);

    simulate_water(&mut grid, initial_source);

    grid.tiles
        .iter()
        .filter(|&tile| matches!(tile, Tile::TrappedWater))
        .count()
}

enum FlowBound {
    Outside(Position),
    Spill(Position),
    Wall(Position),
}

fn find_flow_bounds(grid: &Grid<Tile>, pos: Position) -> (FlowBound, FlowBound) {
    let mut left = pos;
    let left_bound = loop {
        let below = left + Direction::Down;
        if grid.contains(below) && matches!(grid[below], Tile::FlowingWater | Tile::Sand) {
            break FlowBound::Spill(left);
        }
        left = left + Direction::Left;
        if !grid.contains(left) {
            break FlowBound::Outside(left);
        }
        if grid[left] == Tile::Clay {
            break FlowBound::Wall(left + Direction::Right);
        }
    };
    let mut right = pos;
    let right_bound = loop {
        let below = right + Direction::Down;
        if matches!(grid[below], Tile::FlowingWater | Tile::Sand) {
            break FlowBound::Spill(right);
        }
        right = right + Direction::Right;
        if !grid.contains(right) {
            break FlowBound::Outside(right);
        }
        if grid[right] == Tile::Clay {
            break FlowBound::Wall(right + Direction::Left);
        }
    };
    (left_bound, right_bound)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        x=495, y=2..7\n\
        y=7, x=495..501\n\
        x=501, y=3..7\n\
        x=498, y=2..4\n\
        x=506, y=1..2\n\
        x=498, y=10..13\n\
        x=504, y=10..13\n\
        y=13, x=498..504\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            [
                Clay::Vertical(Vertical {
                    x: 495,
                    y1: 2,
                    y2: 7
                }),
                Clay::Horizontal(Horizontal {
                    y: 7,
                    x1: 495,
                    x2: 501
                }),
                Clay::Vertical(Vertical {
                    x: 501,
                    y1: 3,
                    y2: 7
                }),
                Clay::Vertical(Vertical {
                    x: 498,
                    y1: 2,
                    y2: 4
                }),
                Clay::Vertical(Vertical {
                    x: 506,
                    y1: 1,
                    y2: 2
                }),
                Clay::Vertical(Vertical {
                    x: 498,
                    y1: 10,
                    y2: 13
                }),
                Clay::Vertical(Vertical {
                    x: 504,
                    y1: 10,
                    y2: 13
                }),
                Clay::Horizontal(Horizontal {
                    y: 13,
                    x1: 498,
                    x2: 504
                }),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let clay = parse(EXAMPLE).unwrap();
        let result = part_1(&clay);
        assert_eq!(result, 57);
    }

    #[test]
    fn test_part_2() {
        let clay = parse(EXAMPLE).unwrap();
        let result = part_2(&clay);
        assert_eq!(result, 29);
    }
}
