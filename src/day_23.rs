use std::collections::{HashSet, VecDeque};
use std::fmt::Display;
use std::hash::Hash;
use std::num::ParseIntError;
use std::ops::{Add, AddAssign, Mul, MulAssign, RangeInclusive, Sub, SubAssign};
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
struct Vector3 {
    x: i64,
    y: i64,
    z: i64,
}

impl Vector3 {
    #[allow(unused, reason = "tests")]
    const fn new(x: i64, y: i64, z: i64) -> Self {
        Self { x, y, z }
    }

    const fn size(self) -> u64 {
        self.x.unsigned_abs() + self.y.unsigned_abs() + self.z.unsigned_abs()
    }
}

impl AddAssign for Vector3 {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

impl Add for Vector3 {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign for Vector3 {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self.z -= rhs.z;
    }
}

impl Sub for Vector3 {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl MulAssign<i64> for Vector3 {
    fn mul_assign(&mut self, rhs: i64) {
        self.x *= rhs;
        self.y *= rhs;
        self.z *= rhs;
    }
}

impl Mul<i64> for Vector3 {
    type Output = Self;

    fn mul(mut self, rhs: i64) -> Self::Output {
        self *= rhs;
        self
    }
}

impl Display for Vector3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.x)
            .field(&self.y)
            .field(&self.z)
            .finish()
    }
}

impl FromStr for Vector3 {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(',');
        let x = parts
            .next()
            .ok_or(ParseError::SyntaxError)?
            .trim_ascii()
            .parse()?;
        let y = parts
            .next()
            .ok_or(ParseError::SyntaxError)?
            .trim_ascii()
            .parse()?;
        let z = parts
            .next()
            .ok_or(ParseError::SyntaxError)?
            .trim_ascii()
            .parse()?;
        parts
            .next()
            .is_none()
            .then_some(())
            .ok_or(ParseError::SyntaxError)?;
        Ok(Self { x, y, z })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Nanobot {
    center: Vector3,
    radius: u64,
}

impl Nanobot {
    #[allow(unused, reason = "tests")]
    const fn new(center: Vector3, radius: u64) -> Self {
        Self { center, radius }
    }

    fn in_range_of(self, pos: Vector3) -> bool {
        (self.center - pos).size() <= self.radius
    }

    const fn as_span4(&self) -> SpanInclusive4<i64> {
        let &Self {
            center: Vector3 { x, y, z },
            radius,
        } = self;
        let radius = radius.cast_signed();
        SpanInclusive4::new(
            SpanInclusive(-radius - x + y + z, radius - x + y + z),
            SpanInclusive(-radius + x - y + z, radius + x - y + z),
            SpanInclusive(-radius + x + y - z, radius + x + y - z),
            SpanInclusive(-radius + x + y + z, radius + x + y + z),
        )
    }
}

impl FromStr for Nanobot {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (center, radius) = s
            .strip_prefix("pos=<")
            .ok_or(ParseError::SyntaxError)?
            .split_once(">, r=")
            .ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            center: center.parse()?,
            radius: radius.parse()?,
        })
    }
}

#[aoc_generator(day23)]
fn parse(input: &str) -> Result<Vec<Nanobot>, ParseError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day23, part1)]
fn part_1(nanobots: &[Nanobot]) -> usize {
    let strongest = nanobots.iter().max_by_key(|n| n.radius).unwrap();
    nanobots
        .iter()
        .filter(|n| strongest.in_range_of(n.center))
        .count()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SpanInclusive<T>(T, T);

impl<T: Ord + Copy> SpanInclusive<T> {
    fn includes(self, x: T) -> bool {
        self.0 <= x && x <= self.1
    }

    fn intersects_with(self, other: Self) -> bool {
        self.0 <= other.1 && self.1 >= other.0
    }

    fn intersect(self, other: Self) -> Option<Self> {
        self.intersects_with(other)
            .then(|| Self(self.0.max(other.0), self.1.min(other.1)))
    }
}

impl<T> IntoIterator for SpanInclusive<T>
where
    RangeInclusive<T>: Iterator<Item = T>, // `T: Step` is unstable
{
    type Item = T;

    type IntoIter = RangeInclusive<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0..=self.1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SpanInclusive4<T> {
    t: SpanInclusive<T>,
    u: SpanInclusive<T>,
    v: SpanInclusive<T>,
    w: SpanInclusive<T>,
}

impl<T: Ord + Copy> SpanInclusive4<T> {
    const fn new(
        t: SpanInclusive<T>,
        u: SpanInclusive<T>,
        v: SpanInclusive<T>,
        w: SpanInclusive<T>,
    ) -> Self {
        Self { t, u, v, w }
    }

    fn intersect(self, other: Self) -> Option<Self> {
        Some(Self::new(
            self.t.intersect(other.t)?,
            self.u.intersect(other.u)?,
            self.v.intersect(other.v)?,
            self.w.intersect(other.w)?,
        ))
    }
}

#[aoc(day23, part2)]
fn part_2(nanobots: &[Nanobot]) -> u64 {
    let spans = nanobots
        .iter()
        .map(Nanobot::as_span4)
        .collect::<HashSet<_>>();
    let mut max_overlap = Vec::new();
    let mut t_sweeper = LineSweeper::new(&spans, |it| (it.t.0, it.t.1));
    while let Some(t_slice) = t_sweeper.next() {
        if t_slice.len() < max_overlap.len() {
            continue;
        }
        let mut u_sweeper = LineSweeper::new(t_slice, |it| (it.u.0, it.u.1));
        while let Some(u_slice) = u_sweeper.next() {
            if u_slice.len() < max_overlap.len() {
                continue;
            }
            let mut v_sweeper = LineSweeper::new(u_slice, |it| (it.v.0, it.v.1));
            while let Some(v_slice) = v_sweeper.next() {
                if v_slice.len() < max_overlap.len() {
                    continue;
                }
                let mut w_sweeper = LineSweeper::new(v_slice, |it| (it.w.0, it.w.1));
                while let Some(w_slice) = w_sweeper.next() {
                    let overlap = w_slice.len();
                    if overlap < max_overlap.len() {
                        continue;
                    }
                    max_overlap.clear();
                    max_overlap.extend(w_slice.iter().map(|&&&&&it| it));
                }
            }
        }
    }
    let mut min_distance = u64::MAX;
    if let Some(range) = max_overlap
        .iter()
        .map(|&x| Some(x))
        .reduce(|a, b| a?.intersect(b?))
        .flatten()
    {
        for t in range.t {
            for u in range.u {
                for v in range.v {
                    let w = t + u + v;
                    if range.w.includes(w) {
                        let x = (w - t) / 2;
                        let y = (w - u) / 2;
                        let z = (w - v) / 2;
                        let dist = x.unsigned_abs() + y.unsigned_abs() + z.unsigned_abs();
                        min_distance = min_distance.min(dist);
                    }
                }
            }
        }
    }
    min_distance
}

struct LineSweeper<'a, T> {
    active: HashSet<&'a T>,
    events: VecDeque<(i64, SweepEvent<'a, T>)>,
}

impl<'a, T> LineSweeper<'a, T>
where
    T: Eq + Hash + Ord,
{
    fn new(items: &'a HashSet<T>, get_bounds: fn(&T) -> (i64, i64)) -> Self {
        let mut events: Vec<_> = items
            .iter()
            .flat_map(|item| {
                let (start, end) = get_bounds(item);
                [
                    (start, SweepEvent::Enter(item)),
                    (end, SweepEvent::Exit(item)),
                ]
            })
            .collect();
        events.sort_unstable();
        Self {
            active: HashSet::new(),
            events: events.into(),
        }
    }

    fn next(&mut self) -> Option<&HashSet<&'a T>> {
        while let Some((_, event)) = self.events.pop_front() {
            match event {
                SweepEvent::Enter(item) => {
                    self.active.insert(item);
                    return Some(&self.active);
                }
                SweepEvent::Exit(item) => {
                    self.active.remove(item);
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SweepEvent<'a, T> {
    Enter(&'a T),
    Exit(&'a T),
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &str = "\
        pos=<0,0,0>, r=4\n\
        pos=<1,0,0>, r=1\n\
        pos=<4,0,0>, r=3\n\
        pos=<0,2,0>, r=1\n\
        pos=<0,5,0>, r=3\n\
        pos=<0,0,3>, r=1\n\
        pos=<1,1,1>, r=1\n\
        pos=<1,1,2>, r=1\n\
        pos=<1,3,1>, r=1\
    ";

    const EXAMPLE2: &str = "\
        pos=<10,12,12>, r=2\n\
        pos=<12,14,12>, r=2\n\
        pos=<16,12,12>, r=4\n\
        pos=<14,14,14>, r=6\n\
        pos=<50,50,50>, r=200\n\
        pos=<10,10,10>, r=5\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE1).unwrap();
        assert_eq!(
            result,
            [
                Nanobot::new(Vector3::new(0, 0, 0), 4),
                Nanobot::new(Vector3::new(1, 0, 0), 1),
                Nanobot::new(Vector3::new(4, 0, 0), 3),
                Nanobot::new(Vector3::new(0, 2, 0), 1),
                Nanobot::new(Vector3::new(0, 5, 0), 3),
                Nanobot::new(Vector3::new(0, 0, 3), 1),
                Nanobot::new(Vector3::new(1, 1, 1), 1),
                Nanobot::new(Vector3::new(1, 1, 2), 1),
                Nanobot::new(Vector3::new(1, 3, 1), 1),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let nanobots = parse(EXAMPLE1).unwrap();
        let result = part_1(&nanobots);
        assert_eq!(result, 7);
    }

    #[test]
    fn test_part_2() {
        let nanobots = parse(EXAMPLE2).unwrap();
        let result = part_2(&nanobots);
        assert_eq!(result, 36);
    }
}
