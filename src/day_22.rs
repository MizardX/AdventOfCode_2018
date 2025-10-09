use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::num::ParseIntError;
use std::ops::Add;
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
enum RegionType {
    Rocky,
    Wet,
    Narrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Position {
    x: u32,
    y: u32,
}

impl Position {
    const fn new(x: u32, y: u32) -> Self {
        Self { x, y }
    }

    const fn dist(self, other: Self) -> u32 {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
}

impl Add<Direction> for Position {
    type Output = Self;

    fn add(mut self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::North => self.y = self.y.saturating_sub(1),
            Direction::East => self.x += 1,
            Direction::South => self.y += 1,
            Direction::West => self.x = self.x.saturating_sub(1),
        }
        self
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { x, y } = self;
        write!(f, "({x}, {y})")
    }
}

#[derive(Debug, Clone)]
struct Map {
    target_location: TargetLocation,
    cache: HashMap<Position, u32>,
}

impl Map {
    fn new(target_location: TargetLocation) -> Self {
        let cache = HashMap::new();
        Self {
            target_location,
            cache,
        }
    }

    fn geologic_index(&mut self, pos: Position) -> u32 {
        if let Some(&old) = self.cache.get(&pos) {
            return old;
        }
        let res = if pos == self.target_location.position {
            0
        } else if pos.y == 0 {
            pos.x * 16_807
        } else if pos.x == 0 {
            pos.y * 48_271
        } else {
            self.erosion_level(pos + Direction::North) * self.erosion_level(pos + Direction::West)
        } % 20_183;
        self.cache.insert(pos, res);
        res
    }

    fn erosion_level(&mut self, pos: Position) -> u32 {
        (self.geologic_index(pos) + self.target_location.depth) % 20_183
    }

    fn region_type(&mut self, pos: Position) -> RegionType {
        match self.erosion_level(pos) % 3 {
            0 => RegionType::Rocky,
            1 => RegionType::Wet,
            _ => RegionType::Narrow,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TargetLocation {
    depth: u32,
    position: Position,
}

impl FromStr for TargetLocation {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let depth = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("depth: ")
            .ok_or(ParseError::SyntaxError)?;
        let (x, y) = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("target: ")
            .ok_or(ParseError::SyntaxError)?
            .split_once(',')
            .ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            depth: depth.parse()?,
            position: Position {
                x: x.parse()?,
                y: y.parse()?,
            },
        })
    }
}

#[aoc_generator(day22)]
fn parse(input: &str) -> Result<TargetLocation, ParseError> {
    input.parse()
}

#[aoc(day22, part1)]
fn part_1(location: &TargetLocation) -> usize {
    let mut map = Map::new(*location);
    let mut sum = 0;
    for y in 0..=location.position.y {
        for x in 0..=location.position.x {
            let pos = Position::new(x, y);
            sum += match map.region_type(pos) {
                RegionType::Rocky => 0,
                RegionType::Wet => 1,
                RegionType::Narrow => 2,
            };
        }
    }
    sum
}
#[aoc(day22, part2)]
fn part_2(location: &TargetLocation) -> u32 {
    let mut map = Map::new(*location);
    find_shortest_distance(&mut map)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
enum Tool {
    #[default]
    Torch,
    Neither,
    ClimbingGear,
}

impl Tool {
    const fn cycle(self) -> Self {
        match self {
            Self::Neither => Self::ClimbingGear,
            Self::ClimbingGear => Self::Torch,
            Self::Torch => Self::Neither,
        }
    }

    const fn can_be_used_in(self, region_type: RegionType) -> bool {
        matches!(
            (region_type, self),
            (RegionType::Rocky, Self::ClimbingGear | Self::Torch)
                | (RegionType::Wet, Self::Neither | Self::ClimbingGear)
                | (RegionType::Narrow, Self::Neither | Self::Torch)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct State {
    time: u32,
    position: Position,
    tool: Tool,
    target: Position,
}

impl State {
    fn new(target: Position) -> Self {
        Self {
            target,
            ..Default::default()
        }
    }

    fn push_neighbors(self, moves: &mut BinaryHeap<Self>, map: &mut Map) {
        for mut next in [
            Self {
                position: self.position + Direction::North,
                ..self
            },
            Self {
                position: self.position + Direction::East,
                ..self
            },
            Self {
                position: self.position + Direction::South,
                ..self
            },
            Self {
                position: self.position + Direction::West,
                ..self
            },
            Self {
                time: self.time + 6,
                tool: self.tool.cycle(),
                ..self
            },
            Self {
                time: self.time + 6,
                tool: self.tool.cycle().cycle(),
                ..self
            },
        ] {
            next.time += 1;
            if next.tool.can_be_used_in(map.region_type(next.position)) {
                moves.push(next);
            }
        }
    }

    const fn heuristic(&self) -> u32 {
        self.time + self.position.dist(self.target)
    }

    const fn is_goal(&self) -> bool {
        self.position.dist(self.target) == 0 && matches!(self.tool, Tool::Torch)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.heuristic().cmp(&self.heuristic())
    }
}

fn find_shortest_distance(map: &mut Map) -> u32 {
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();

    queue.push(State::new(map.target_location.position));

    while let Some(state) = queue.pop() {
        if !visited.insert((state.position, state.tool)) {
            continue;
        }

        if state.is_goal() {
            return state.time;
        }

        state.push_neighbors(&mut queue, map);
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        depth: 510\n\
        target: 10,10\
    ";

    fn parse_example() -> TargetLocation {
        parse(EXAMPLE).unwrap()
    }

    #[test]
    fn test_parse() {
        let result = parse_example();
        assert_eq!(result.depth, 510);
        assert_eq!(result.position, Position::new(10, 10));
    }

    #[test]
    fn test_map_calculations() {
        let mut map = Map::new(parse_example());
        let positions = [
            Position::new(0, 0),
            Position::new(1, 0),
            Position::new(0, 1),
            Position::new(1, 1),
            map.target_location.position,
        ];
        let geologic_indexes = positions.map(|pos| {
            (
                map.geologic_index(pos),
                map.erosion_level(pos),
                map.region_type(pos),
            )
        });
        #[allow(clippy::identity_op)]
        let expected = [
            (0_u32, 510, RegionType::Rocky),
            (16_807 % 20_183, 17_317, RegionType::Wet),
            (48_271 % 20_183, 8415, RegionType::Rocky),
            (8_415 * 17_317 % 20_183, 1805, RegionType::Narrow),
            (0_u32, 510, RegionType::Rocky),
        ];
        assert_eq!(geologic_indexes, expected);
    }

    #[test]
    fn test_region_type_map() {
        let mut map = Map::new(parse_example());
        let mut display = String::new();
        for y in 0..16 {
            if y > 0 {
                display.push('\n');
            }
            for x in 0..16 {
                display.push(match map.region_type(Position::new(x, y)) {
                    RegionType::Rocky => '.',
                    RegionType::Wet => '=',
                    RegionType::Narrow => '|',
                });
            }
        }
        let expected = "\
            .=.|=.|.|=.|=|=.\n\
            .|=|=|||..|.=...\n\
            .==|....||=..|==\n\
            =.|....|.==.|==.\n\
            =|..==...=.|==..\n\
            =||.=.=||=|=..|=\n\
            |.=.===|||..=..|\n\
            |..==||=.|==|===\n\
            .=..===..=|.|||.\n\
            .======|||=|=.|=\n\
            .===|=|===.===||\n\
            =|||...|==..|=.|\n\
            =.=|=.=..=.||==|\n\
            ||=|=...|==.=|==\n\
            |=.=||===.|||===\n\
            ||.|==.|.|.||=||\
        ";
        assert_eq!(
            display, expected,
            "Result:\n{display}\nExpected:\n{expected}"
        );
    }

    #[test]
    fn test_part_1() {
        let location = parse_example();
        let result = part_1(&location);
        assert_eq!(result, 114);
    }

    #[test]
    fn test_part_2() {
        let location = parse_example();
        let result = part_2(&location);
        assert_eq!(result, 45);
    }
}
