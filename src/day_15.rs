use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::{Debug, Display};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Tile {
    Open,
    Wall,
}

impl Tile {
    fn is_passable(self) -> bool {
        self == Self::Open
    }
}

impl TryFrom<u8> for Tile {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'#' => Self::Wall,
            b'.' => Self::Open,
            _ => return Err(ParseError::InvalidTile),
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
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

impl Direction {
    const fn all() -> [Self; 4] {
        [Self::Up, Self::Left, Self::Right, Self::Down]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Elf,
    Goblin,
}

impl TryFrom<u8> for Kind {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'E' => Self::Elf,
            b'G' => Self::Goblin,
            _ => return Err(ParseError::InvalidTile),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Unit {
    id: usize,
    pos: Position,
    kind: Kind,
    hit_points: u32,
    attack_power: u32,
}

impl Unit {
    const fn new(id: usize, pos: Position, kind: Kind) -> Self {
        Self {
            id,
            pos,
            kind,
            hit_points: 200,
            attack_power: 3,
        }
    }

    const fn is_dead(&self) -> bool {
        self.hit_points == 0
    }
    const fn is_alive(&self) -> bool {
        self.hit_points > 0
    }

    const fn is_enemy_of(&self, other: &Self) -> bool {
        matches!(
            (self.kind, other.kind),
            (Kind::Elf, Kind::Goblin) | (Kind::Goblin, Kind::Elf)
        )
    }

    const fn take_damage(&mut self, damage: u32) {
        self.hit_points = self.hit_points.saturating_sub(damage);
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let &Self {
            id,
            kind,
            hit_points,
            ..
        } = self;
        match kind {
            Kind::Elf => write!(f, "E{id}({hit_points})"),
            Kind::Goblin => write!(f, "G{id}({hit_points})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Cavern {
    grid: Grid<Tile>,
    units: Vec<Unit>,
}

impl FromStr for Cavern {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rows = 0;
        let mut cols = 0;
        for line in s.lines() {
            rows += 1;
            cols = line.len();
        }
        let mut grid = Grid::new(rows, cols, |_, _| MaybeUninit::uninit());
        let mut units = Vec::new();
        for (row, line) in s.lines().enumerate() {
            if line.len() != cols {
                return Err(ParseError::JaggedGrid);
            }
            for (col, ch) in line.bytes().enumerate() {
                let pos = Position { row, col };
                grid[pos] = match ch {
                    b'E' | b'G' => {
                        units.push(Unit::new(units.len() + 1, pos, ch.try_into()?));
                        MaybeUninit::new(Tile::Open)
                    }
                    _ => MaybeUninit::new(ch.try_into()?),
                };
            }
        }
        // SAFETY: MaybeUninit<T> is guaranteed to be transmutable to T if it does not have a strange memory layout.
        let grid = unsafe { std::mem::transmute::<Grid<MaybeUninit<Tile>>, Grid<Tile>>(grid) };
        Ok(Self { grid, units })
    }
}

#[aoc_generator(day15)]
fn parse(input: &str) -> Result<Cavern, ParseError> {
    input.parse()
}

#[derive(Debug, Clone)]
struct BattleSimulator<'a> {
    cavern: &'a Cavern,
    units: Vec<Unit>,
    lookup: HashMap<Position, usize>,
    queue: BinaryHeap<Reverse<(Position, usize)>>,
    round: u32,
    log: bool,
}

impl<'a> BattleSimulator<'a> {
    fn new(cavern: &'a Cavern) -> Self {
        let queue = BinaryHeap::new();
        let mut lookup = HashMap::new();
        let units = cavern.units.clone();
        for (index, unit) in units.iter().enumerate() {
            lookup.insert(unit.pos, index);
        }
        Self {
            cavern,
            units,
            lookup,
            queue,
            round: 0,
            log: false,
        }
    }

    fn reset(&mut self) {
        self.queue.clear();
        self.lookup.clear();
        self.units.clear();
        self.round = 0;
        self.units.extend_from_slice(&self.cavern.units);
        for (index, unit) in self.units.iter().enumerate() {
            self.lookup.insert(unit.pos, index);
        }
    }

    fn execute_full_battle(&mut self) -> u32 {
        while self.execute_round() == Some(true) {}
        if self.log {
            print!("Remaining units:");
        }
        let mut total_hp = 0;
        for unit in &self.units {
            if unit.is_alive() {
                if self.log {
                    print!(" {unit}");
                }
                total_hp += unit.hit_points;
            }
        }
        let checksum = (self.round - 1) * total_hp;
        if self.log {
            println!(
                ". Final Round = {}. Total HP = {total_hp}. Checksum = {checksum}",
                self.round
            );
        }
        checksum
    }

    fn enqueue_units(&mut self) {
        self.queue.clear();
        for (index, unit) in self.units.iter().enumerate() {
            if unit.is_alive() {
                self.queue.push(Reverse((unit.pos, index)));
            }
        }
    }

    fn execute_round(&mut self) -> Option<bool> {
        self.round += 1;
        if self.log {
            println!("{self}");
            println!("== Round {} ==", self.round);
        }
        self.enqueue_units();
        while self.move_next_unit()? {}

        Some(true)
    }

    fn move_next_unit(&mut self) -> Option<bool> {
        while let Some(&Reverse((_, index))) = self.queue.peek()
            && self.units[index].is_dead()
        {
            self.queue.pop();
        }
        let Some(Reverse((mut pos, index))) = self.queue.pop() else {
            return Some(false);
        };

        // Check if all enemies are dead
        let unit_kind = self.units[index].kind;
        if self
            .units
            .iter()
            .all(|u| u.kind == unit_kind || u.is_dead())
        {
            // Game does not end at the end of a round when all of one kind are dead.
            // Game ends only when one unit can't find any enemy to fight.
            // So if a the last unit in a round kills its last enemy, the game continues to the
            // next round anyway.
            // Returning `None` here exists `move_next_unit()`, `execute_round()` and the loop in
            // `execute_full_battle()`, effectively ending the game. 
            if self.log {
                println!(
                    "All enemies of {} are dead. Battle is over.",
                    self.units[index]
                );
            }
            return None;
        }

        // 1. Move towards enemy
        let moved = if let Some(dir) = self.find_direction_of_next_victim(&self.units[index]) {
            self.lookup.remove(&pos);
            pos = pos + dir;
            if self.log {
                println!("{} moving to {pos:?}", &self.units[index]);
            }
            self.units[index].pos = pos;
            self.lookup.insert(pos, index);
            true
        } else {
            false
        };

        // 1. Attack enemy if in reach.
        let mut target = None;
        for dir in Direction::all() {
            let pos1 = pos + dir;
            if self.cavern.grid[pos1].is_passable()
                && let Some(&enemy_ix) = self.lookup.get(&pos1)
                && self.units[enemy_ix].is_enemy_of(&self.units[index])
                && target.is_none_or(|(hp, _)| hp > self.units[enemy_ix].hit_points)
            {
                target = Some((self.units[enemy_ix].hit_points, enemy_ix));
            }
        }

        if let Some((_, ix)) = target {
            // Attack enemy

            if self.log {
                print!("{} attacking {}", &self.units[index], &self.units[ix]);
            }
            let damage = self.units[index].attack_power;
            self.units[ix].take_damage(damage);
            if self.log {
                println!(" -> {}", self.units[ix].hit_points);
            }
            if self.units[ix].is_dead() {
                if self.log {
                    println!("{} died", &self.units[ix]);
                }
                self.lookup.remove(&self.units[ix].pos);
            }
            return Some(true);
        }

        if !moved && self.log {
            println!("{} is idle", &self.units[index]);
        }

        Some(true)
    }

    fn find_direction_of_next_victim(&self, source: &Unit) -> Option<Direction> {
        let mut visited = HashSet::new();
        visited.insert(source.pos);
        let mut pending = BinaryHeap::<Reverse<(usize, Position, Direction)>>::new();
        for dir in Direction::all() {
            let pos1 = source.pos + dir;
            if self.cavern.grid[pos1].is_passable() {
                if let Some(&enemy_ix) = self.lookup.get(&pos1)
                    && self.units[enemy_ix].is_alive()
                    && self.units[enemy_ix].is_enemy_of(source)
                {
                    // Already beside an enemy
                    return None;
                }
                pending.push(Reverse((1, pos1, dir)));
            }
        }
        while let Some(Reverse((dist, pos, start_dir))) = pending.pop() {
            if !visited.insert(pos) {
                continue;
            }
            if let Some(&enemy_ix) = self.lookup.get(&pos) {
                if self.units[enemy_ix].is_alive() && self.units[enemy_ix].is_enemy_of(source) {
                    return Some(start_dir);
                }
                // Friedlies are ignored, since we can't walk through them
            } else {
                for dir in Direction::all() {
                    let pos1 = pos + dir;
                    if !visited.contains(&pos1) && self.cavern.grid[pos1].is_passable() {
                        pending.push(Reverse((dist + 1, pos1, start_dir)));
                    }
                }
            }
        }
        None
    }
}

impl Display for BattleSimulator<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cavern = self.cavern;
        for row in 0..cavern.grid.rows {
            if row > 0 {
                writeln!(f)?;
            }
            for col in 0..cavern.grid.cols {
                let pos = Position { row, col };
                write!(
                    f,
                    "{}",
                    if let Some(&ix) = self.lookup.get(&pos) {
                        match &self.units[ix] {
                            Unit { hit_points: 0, .. } => 'X',
                            Unit {
                                kind: Kind::Elf, ..
                            } => 'E',
                            Unit {
                                kind: Kind::Goblin, ..
                            } => 'G',
                        }
                    } else {
                        match cavern.grid[pos] {
                            Tile::Open => '.',
                            Tile::Wall => '#',
                        }
                    }
                )?;
            }
            for col in 0..cavern.grid.cols {
                let pos = Position { row, col };
                if let Some(&ix) = self.lookup.get(&pos)
                    && self.units[ix].is_alive()
                {
                    write!(f, " {}", &self.units[ix])?;
                }
            }
        }
        Ok(())
    }
}

#[aoc(day15, part1)]
fn part_1(cavern: &Cavern) -> u32 {
    let mut sim = BattleSimulator::new(cavern);
    // sim.log = true;
    sim.execute_full_battle()
}

#[aoc(day15, part2)]
fn part_2(cavern: &Cavern) -> u32 {
    let mut sim = BattleSimulator::new(cavern);
    // sim.log = true;
    for elf_power in 4..=200 {
        sim.reset();
        for unit in &mut sim.units {
            if unit.kind == Kind::Elf {
                unit.attack_power = elf_power;
            }
        }
        let outcome = sim.execute_full_battle();
        if sim
            .units
            .iter()
            .all(|u| (u.kind == Kind::Elf) == u.is_alive())
        {
            return outcome;
        }
    }
    unreachable!("Killing in a single hit is not enough?")
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    const EXAMPLE_STEP: &str = "\
        #######\n\
        #.E...#\n\
        #.....#\n\
        #...G.#\n\
        #######\
    ";
    const EXAMPLE_STEP_LARGE: &str = "\
        #########\n\
        #G..G..G#\n\
        #.......#\n\
        #.......#\n\
        #G..E..G#\n\
        #.......#\n\
        #.......#\n\
        #G..G..G#\n\
        #########\
    ";

    const EXAMPLE_ATTACK: &str = "\
        G....\n\
        ..G..\n\
        ..EG.\n\
        ..G..\n\
        ...G.\
    ";

    #[allow(unused)]
    const EXAMPLE1: &str = "\
        #######\n\
        #.G...#\n\
        #...EG#\n\
        #.#.#G#\n\
        #..G#E#\n\
        #.....#\n\
        #######\
    ";

    const EXAMPLE2: &str = "\
        #######\n\
        #G..#E#\n\
        #E#E.E#\n\
        #G.##.#\n\
        #...#E#\n\
        #...E.#\n\
        #######\
    ";

    const EXAMPLE3: &str = "\
        #######\n\
        #E..EG#\n\
        #.#G.E#\n\
        #E.##E#\n\
        #G..#.#\n\
        #..E#.#\n\
        #######\
    ";

    const EXAMPLE4: &str = "\
        #######\n\
        #E.G#.#\n\
        #.#G..#\n\
        #G.#.G#\n\
        #G..#.#\n\
        #...E.#\n\
        #######\
    ";

    const EXAMPLE5: &str = "\
        #######\n\
        #.E...#\n\
        #.#..G#\n\
        #.###.#\n\
        #E#G#G#\n\
        #...#G#\n\
        #######\
    ";

    const EXAMPLE6: &str = "\
        #########\n\
        #G......#\n\
        #.E.#...#\n\
        #..##..G#\n\
        #...##..#\n\
        #...#...#\n\
        #.G...G.#\n\
        #.....G.#\n\
        #########\
    ";

    macro_rules! pos {
        ($row:expr,$col:expr) => {
            Position {
                row: $row,
                col: $col,
            }
        };
    }

    #[test]
    fn test_step() {
        let cavern = parse(EXAMPLE_STEP).unwrap();
        let mut sim = BattleSimulator::new(&cavern);
        sim.enqueue_units();
        assert_eq!(sim.units[0].pos, pos!(1, 2));
        sim.move_next_unit();
        assert_eq!(sim.units[0].pos, pos!(1, 3));
    }

    #[test]
    fn test_step_large() {
        let cavern = parse(EXAMPLE_STEP_LARGE).unwrap();
        let mut sim = BattleSimulator::new(&cavern);
        assert_eq!(
            sim.units.iter().map(|u| u.pos).collect::<Vec<_>>(),
            [
                pos!(1, 1),
                pos!(1, 4),
                pos!(1, 7),
                pos!(4, 1),
                pos!(4, 4),
                pos!(4, 7),
                pos!(7, 1),
                pos!(7, 4),
                pos!(7, 7),
            ],
            "Initial"
        );
        sim.execute_round();
        assert_eq!(
            sim.units.iter().map(|u| u.pos).collect::<Vec<_>>(),
            [
                pos!(1, 2),
                pos!(2, 4),
                pos!(1, 6),
                pos!(4, 2),
                pos!(3, 4),
                pos!(3, 7),
                pos!(6, 1),
                pos!(6, 4),
                pos!(6, 7),
            ],
            "After 1 round"
        );
        sim.execute_round();
        assert_eq!(
            sim.units.iter().map(|u| u.pos).collect::<Vec<_>>(),
            [
                pos!(1, 3),
                pos!(2, 4),
                pos!(1, 5),
                pos!(3, 2),
                pos!(3, 4),
                pos!(3, 6),
                pos!(5, 1),
                pos!(5, 4),
                pos!(5, 7),
            ],
            "After 2 rounds"
        );
        sim.execute_round();
        assert_eq!(
            sim.units.iter().map(|u| u.pos).collect::<Vec<_>>(),
            [
                pos!(2, 3),
                pos!(2, 4),
                pos!(2, 5),
                pos!(3, 3),
                pos!(3, 4),
                pos!(3, 5),
                pos!(4, 1),
                pos!(4, 4),
                pos!(5, 7),
            ],
            "After 3 rounds"
        );
    }

    #[test]
    fn test_attack() {
        let mut cavern = parse(EXAMPLE_ATTACK).unwrap();
        cavern.units[0].hit_points = 9;
        cavern.units[1].hit_points = 4;
        cavern.units[3].hit_points = 2;
        cavern.units[4].hit_points = 2;
        cavern.units[5].hit_points = 1;
        let mut sim = BattleSimulator::new(&cavern);
        println!("{sim}");
        sim.log = true;
        // Skip to the elf (index 2)
        sim.enqueue_units();
        while let Some(&Reverse((_, next))) = sim.queue.peek()
            && next != 2
        {
            sim.queue.pop().unwrap();
        }
        assert_eq!(sim.units[3].hit_points, 2);
        sim.move_next_unit();
        assert_eq!(sim.units[3].hit_points, 0);
    }

    #[test_case(EXAMPLE1 => 27_730)]
    #[test_case(EXAMPLE2 => 36_334)]
    #[test_case(EXAMPLE3 => 39_514)]
    #[test_case(EXAMPLE4 => 27_755)]
    #[test_case(EXAMPLE5 => 28_944)]
    #[test_case(EXAMPLE6 => 18_740)]
    fn test_part_1(input: &str) -> u32 {
        let cavern = parse(input).unwrap();
        part_1(&cavern)
    }

    // EXAMPLE 2 is not used here

    #[test_case(EXAMPLE1 => 4_988)]
    #[test_case(EXAMPLE3 => 31_284)]
    #[test_case(EXAMPLE4 => 3_478)]
    #[test_case(EXAMPLE5 => 6_474)]
    #[test_case(EXAMPLE6 => 1_140)]
    fn test_part_2(input: &str) -> u32 {
        let cavern = parse(input).unwrap();
        part_2(&cavern)
    }
}
