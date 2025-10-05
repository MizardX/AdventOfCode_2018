use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::{Display, Write};
use std::mem::MaybeUninit;
use std::ops::{Add, Index, IndexMut};
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Invalid tile")]
    InvalidTile,
    #[error("Grid rows are jagged")]
    JaggedGrid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
enum Direction {
    #[default]
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    const fn clockwise(self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }

    const fn counterclockwise(self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Right => Self::Up,
            Self::Down => Self::Right,
            Self::Left => Self::Down,
        }
    }
}

impl TryFrom<u8> for Direction {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'^' => Self::Up,
            b'>' => Self::Right,
            b'v' => Self::Down,
            b'<' => Self::Left,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Orientation {
    Horizontal,
    Vertical,
}

impl TryFrom<u8> for Orientation {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'-' => Self::Horizontal,
            b'|' => Self::Vertical,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

impl From<Direction> for Orientation {
    fn from(value: Direction) -> Self {
        match value {
            Direction::Down | Direction::Up => Self::Vertical,
            Direction::Left | Direction::Right => Self::Horizontal,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Corner {
    BottomLeftOrTopRight,
    TopLeftOrBottomRight,
}

impl TryFrom<u8> for Corner {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'/' => Self::TopLeftOrBottomRight,
            b'\\' => Self::BottomLeftOrTopRight,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Tile {
    Blank,
    Straight(Orientation),
    Corner(Corner),
    Intersection,
}

impl TryFrom<u8> for Tile {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b' ' => Self::Blank,
            b'-' | b'|' => Self::Straight(value.try_into()?),
            b'/' | b'\\' => Self::Corner(value.try_into()?),
            b'+' => Self::Intersection,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match *self {
            Self::Blank => ' ',
            Self::Straight(Orientation::Horizontal) => '-',
            Self::Straight(Orientation::Vertical) => '|',
            Self::Corner(Corner::TopLeftOrBottomRight) => '/',
            Self::Corner(Corner::BottomLeftOrTopRight) => '\\',
            Self::Intersection => '+',
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
}

impl<T> Index<Position> for Grid<T> {
    type Output = T;

    fn index(&self, index: Position) -> &Self::Output {
        &self.tiles[index.row * self.cols + index.col]
    }
}

impl<T> IndexMut<Position> for Grid<T> {
    fn index_mut(&mut self, index: Position) -> &mut Self::Output {
        &mut self.tiles[index.row * self.cols + index.col]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Position {
    row: usize,
    col: usize,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Action {
    #[default]
    Left,
    Straight,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Cart {
    direction: Direction,
    position: Position,
    turn_action: Action,
    dead: bool,
}

impl Cart {
    fn tick(&mut self, grid: &Grid<Tile>) -> Position {
        self.position = self.position + self.direction;
        match grid[self.position] {
            Tile::Corner(corner) => {
                self.direction = match (corner, self.direction) {
                    (Corner::BottomLeftOrTopRight, Direction::Right)
                    | (Corner::TopLeftOrBottomRight, Direction::Left) => Direction::Down,
                    (Corner::BottomLeftOrTopRight, Direction::Up)
                    | (Corner::TopLeftOrBottomRight, Direction::Down) => Direction::Left,
                    (Corner::BottomLeftOrTopRight, Direction::Down)
                    | (Corner::TopLeftOrBottomRight, Direction::Up) => Direction::Right,
                    (Corner::BottomLeftOrTopRight, Direction::Left)
                    | (Corner::TopLeftOrBottomRight, Direction::Right) => Direction::Up,
                }
            }
            Tile::Intersection => {
                (self.turn_action, self.direction) = match self.turn_action {
                    Action::Left => (Action::Straight, self.direction.counterclockwise()),
                    Action::Straight => (Action::Right, self.direction),
                    Action::Right => (Action::Left, self.direction.clockwise()),
                }
            }
            Tile::Blank => panic!("Entered blank space!"),
            Tile::Straight(..) => {}
        }
        self.position
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TrackSystem {
    grid: Grid<Tile>,
    carts: Vec<Cart>,
}

impl FromStr for TrackSystem {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rows = 0;
        let mut cols = 0;
        for line in s.lines() {
            rows += 1;
            cols = line.len();
        }
        let mut grid = Grid::new(rows, cols, |_, _| MaybeUninit::uninit());
        let mut carts = Vec::new();
        for (row, line) in s.lines().enumerate() {
            if line.len() != cols {
                return Err(ParseError::JaggedGrid);
            }
            for (col, ch) in line.bytes().enumerate() {
                grid[Position { row, col }] = match ch {
                    b'<' | b'^' | b'>' | b'v' => {
                        let cart = Cart {
                            direction: ch.try_into()?,
                            position: Position { row, col },
                            ..Default::default()
                        };
                        carts.push(cart);
                        MaybeUninit::new(Tile::Straight(cart.direction.into()))
                    }
                    _ => MaybeUninit::new(ch.try_into()?),
                };
            }
        }
        // SAFETY: MaybeUninit<T> is guaranteed to be transmutable to T if it does not have a strange memory layout.
        let grid = unsafe { std::mem::transmute::<Grid<MaybeUninit<Tile>>, Grid<Tile>>(grid) };
        Ok(Self { grid, carts })
    }
}

#[aoc_generator(day13)]
fn parse(input: &str) -> Result<TrackSystem, ParseError> {
    input.parse()
}

#[aoc(day13, part1)]
fn part_1(system: &TrackSystem) -> String {
    simulate(system, false)
}

#[aoc(day13, part2)]
fn part_2(system: &TrackSystem) -> String {
    simulate(system, true)
}

fn simulate(system: &TrackSystem, part_2: bool) -> String {
    let mut carts = system.carts.clone();
    let mut pending = BinaryHeap::new();
    let mut positions = HashMap::new();
    for (index, cart) in carts.iter().enumerate() {
        positions.insert(cart.position, index);
        pending.push(Reverse((cart.position, index)));
    }
    let mut next = Vec::new();
    let mut num_alive = carts.len();
    for _ in 0..100_000 {
        while let Some(Reverse((pos, index))) = pending.pop() {
            if carts[index].dead {
                continue;
            }
            positions.remove(&pos);
            let new_pos = carts[index].tick(&system.grid);
            if let Some(victim) = positions.remove(&new_pos) {
                if !part_2 {
                    let Position { row, col } = new_pos;
                    return format!("{col},{row}");
                }
                carts[victim].dead = true;
                carts[index].dead = true;
                num_alive -= 2;
                continue;
            }
            positions.insert(new_pos, index);
            next.push(Reverse((new_pos, index)));
        }
        pending.extend(&next);
        next.clear();
        if num_alive <= 1 {
            break;
        }
    }
    if num_alive > 1 {
        unreachable!("Still alive after 100.000 cycles?");
    }
    for cart in &carts {
        if cart.dead {
            continue;
        }
        let Position { row, col } = cart.position;
        return format!("{col},{row}");
    }
    unreachable!("All dead?");
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &str = r"
/->-\........
|...|../----\
|./-+--+-\..|
|.|.|..|.v..|
\-+-/..\-+--/
..\------/...
"
    .trim_ascii();

    const EXAMPLE2: &str = r"
/>-<\..
|...|..
|./<+-\
|.|.|.v
\>+</.|
..|...^
..\<->/
"
    .trim_ascii();

    fn parse_example(input: &str) -> TrackSystem {
        let masaged_input = input.replace('.', " ");
        parse(&masaged_input).unwrap()
    }

    #[test]
    fn test_parse() {
        let result = parse_example(EXAMPLE1);
        macro_rules! pos {
            ($row:expr,$col:expr) => {
                Position {
                    row: $row,
                    col: $col,
                }
            };
        }
        macro_rules! cart {
            ($pos:expr, $dir:expr) => {
                Cart {
                    position: $pos,
                    direction: $dir,
                    ..Default::default()
                }
            };
        }
        assert_eq!(
            result.carts,
            [
                cart!(pos!(0, 2), Direction::Right),
                cart!(pos!(3, 9), Direction::Down)
            ]
        );
        assert_eq!(result.grid.rows, 6);
        assert_eq!(result.grid.cols, 13);
        // Sample each tile type
        let positions = [
            pos!(0, 0),
            pos!(0, 1),
            pos!(0, 2),
            pos!(0, 4),
            pos!(0, 5),
            pos!(1, 0),
        ]
        .map(|pos| result.grid[pos]);
        assert_eq!(
            positions,
            [
                Tile::Corner(Corner::TopLeftOrBottomRight),
                Tile::Straight(Orientation::Horizontal),
                Tile::Straight(Orientation::Horizontal),
                Tile::Corner(Corner::BottomLeftOrTopRight),
                Tile::Blank,
                Tile::Straight(Orientation::Vertical),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let system = parse_example(EXAMPLE1);
        let result = part_1(&system);
        assert_eq!(result, "7,3");
    }

    #[test]
    fn test_part_2() {
        let system = parse_example(EXAMPLE2);
        let result = part_2(&system);
        assert_eq!(result, "6,4");
    }
}
