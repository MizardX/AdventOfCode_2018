use std::num::ParseIntError;
use std::ops::{Add, Mul, Sub};
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
struct Vector {
    x: i64,
    y: i64,
}

impl Add for Vector {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.x += rhs.x;
        self.y += rhs.y;
        self
    }
}

impl Sub for Vector {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self
    }
}

impl Mul<i64> for Vector {
    type Output = Self;

    fn mul(mut self, rhs: i64) -> Self::Output {
        self.x *= rhs;
        self.y *= rhs;
        self
    }
}

impl FromStr for Vector {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s.split_once(',').ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            x: x.trim_ascii().parse()?,
            y: y.trim_ascii().parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Point {
    position: Vector,
    velocity: Vector,
}

impl Point {
    fn project(&self, time: i64) -> Vector {
        self.position + self.velocity * time
    }
}

impl FromStr for Point {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rest = s
            .strip_prefix("position=<")
            .ok_or(ParseError::SyntaxError)?;
        let (position, rest) = rest
            .split_once("> velocity=<")
            .ok_or(ParseError::SyntaxError)?;
        let velocity = rest.strip_suffix(">").ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            position: position.parse()?,
            velocity: velocity.parse()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NightSky {
    points: Vec<Point>,
}

impl NightSky {
    fn project(&self, time: i64) -> String {
        let mut min_x = i64::MAX;
        let mut max_x = i64::MIN;
        let mut min_y = i64::MAX;
        let mut max_y = i64::MIN;
        for point in &self.points {
            let pos = point.project(time);
            min_x = min_x.min(pos.x);
            max_x = max_x.max(pos.x);
            min_y = min_y.min(pos.y);
            max_y = max_y.max(pos.y);
        }
        let width = usize::try_from(max_x - min_x + 1).unwrap();
        let height = usize::try_from(max_y - min_y + 1).unwrap();
        let mut grid = vec![vec![false; width]; height];
        for point in &self.points {
            let pos = point.project(time);
            grid[usize::try_from(pos.y - min_y).unwrap()]
                [usize::try_from(pos.x - min_x).unwrap()] = true;
        }
        let mut display = String::new();
        display.push('\n'); // to start in the leftmost column
        for (above, below) in grid.iter().zip(&grid[1..]).step_by(2) {
            for (&pixel1, &pixel2) in above.iter().zip(below) {
                display.push(match (pixel1, pixel2) {
                    (false, false) => ' ',
                    (false, true) => '▄',
                    (true, false) => '▀',
                    (true, true) => '█',
                });
            }
            display.push('\n');
        }
        display
    }
}

impl FromStr for NightSky {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let points = s.lines().map(str::parse).collect::<Result<_, _>>()?;
        Ok(Self { points })
    }
}

#[aoc_generator(day10)]
fn parse(input: &str) -> Result<NightSky, ParseError> {
    input.parse()
}

#[aoc(day10, part1)]
fn part_1(night_sky: &NightSky) -> String {
    animate_sky(night_sky).0
}

#[aoc(day10, part2)]
fn part_2(night_sky: &NightSky) -> i64 {
    animate_sky(night_sky).1
}

fn animate_sky(night_sky: &NightSky) -> (String, i64) {
    let mut min_area = i64::MAX;
    for time in 0.. {
        let mut min_x = i64::MAX;
        let mut max_x = i64::MIN;
        let mut min_y = i64::MAX;
        let mut max_y = i64::MIN;
        for point in &night_sky.points {
            let pos = point.project(time);
            min_x = min_x.min(pos.x);
            max_x = max_x.max(pos.x);
            min_y = min_y.min(pos.y);
            max_y = max_y.max(pos.y);
        }
        let area = (max_x - min_x + 1) * (max_y - min_y + 1);
        if area < min_area {
            min_area = area;
        } else if min_area < 1_000_000 {
            return (night_sky.project(time - 1), time - 1);
        }
    }
    (String::new(), 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        position=< 9,  1> velocity=< 0,  2>\n\
        position=< 7,  0> velocity=<-1,  0>\n\
        position=< 3, -2> velocity=<-1,  1>\n\
        position=< 6, 10> velocity=<-2, -1>\n\
        position=< 2, -4> velocity=< 2,  2>\n\
        position=<-6, 10> velocity=< 2, -2>\n\
        position=< 1,  8> velocity=< 1, -1>\n\
        position=< 1,  7> velocity=< 1,  0>\n\
        position=<-3, 11> velocity=< 1, -2>\n\
        position=< 7,  6> velocity=<-1, -1>\n\
        position=<-2,  3> velocity=< 1,  0>\n\
        position=<-4,  3> velocity=< 2,  0>\n\
        position=<10, -3> velocity=<-1,  1>\n\
        position=< 5, 11> velocity=< 1, -2>\n\
        position=< 4,  7> velocity=< 0, -1>\n\
        position=< 8, -2> velocity=< 0,  1>\n\
        position=<15,  0> velocity=<-2,  0>\n\
        position=< 1,  6> velocity=< 1,  0>\n\
        position=< 8,  9> velocity=< 0, -1>\n\
        position=< 3,  3> velocity=<-1,  1>\n\
        position=< 0,  5> velocity=< 0, -1>\n\
        position=<-2,  2> velocity=< 2,  0>\n\
        position=< 5, -2> velocity=< 1,  2>\n\
        position=< 1,  4> velocity=< 2,  1>\n\
        position=<-2,  7> velocity=< 2, -2>\n\
        position=< 3,  6> velocity=<-1, -1>\n\
        position=< 5,  0> velocity=< 1,  0>\n\
        position=<-6,  0> velocity=< 2,  0>\n\
        position=< 5,  9> velocity=< 1, -2>\n\
        position=<14,  7> velocity=<-2,  0>\n\
        position=<-3,  6> velocity=< 2, -1>\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        macro_rules! v {
            ($x:expr, $y:expr) => {
                Vector { x: $x, y: $y }
            };
        }
        macro_rules! point {
            (pos = $pos:expr, vel = $vel:expr) => {
                Point {
                    position: $pos,
                    velocity: $vel,
                }
            };
        }
        assert_eq!(
            result.points,
            [
                point!(pos = v!(9, 1), vel = v!(0, 2)),
                point!(pos = v!(7, 0), vel = v!(-1, 0)),
                point!(pos = v!(3, -2), vel = v!(-1, 1)),
                point!(pos = v!(6, 10), vel = v!(-2, -1)),
                point!(pos = v!(2, -4), vel = v!(2, 2)),
                point!(pos = v!(-6, 10), vel = v!(2, -2)),
                point!(pos = v!(1, 8), vel = v!(1, -1)),
                point!(pos = v!(1, 7), vel = v!(1, 0)),
                point!(pos = v!(-3, 11), vel = v!(1, -2)),
                point!(pos = v!(7, 6), vel = v!(-1, -1)),
                point!(pos = v!(-2, 3), vel = v!(1, 0)),
                point!(pos = v!(-4, 3), vel = v!(2, 0)),
                point!(pos = v!(10, -3), vel = v!(-1, 1)),
                point!(pos = v!(5, 11), vel = v!(1, -2)),
                point!(pos = v!(4, 7), vel = v!(0, -1)),
                point!(pos = v!(8, -2), vel = v!(0, 1)),
                point!(pos = v!(15, 0), vel = v!(-2, 0)),
                point!(pos = v!(1, 6), vel = v!(1, 0)),
                point!(pos = v!(8, 9), vel = v!(0, -1)),
                point!(pos = v!(3, 3), vel = v!(-1, 1)),
                point!(pos = v!(0, 5), vel = v!(0, -1)),
                point!(pos = v!(-2, 2), vel = v!(2, 0)),
                point!(pos = v!(5, -2), vel = v!(1, 2)),
                point!(pos = v!(1, 4), vel = v!(2, 1)),
                point!(pos = v!(-2, 7), vel = v!(2, -2)),
                point!(pos = v!(3, 6), vel = v!(-1, -1)),
                point!(pos = v!(5, 0), vel = v!(1, 0)),
                point!(pos = v!(-6, 0), vel = v!(2, 0)),
                point!(pos = v!(5, 9), vel = v!(1, -2)),
                point!(pos = v!(14, 7), vel = v!(-2, 0)),
                point!(pos = v!(-3, 6), vel = v!(2, -1)),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let night_sky = parse(EXAMPLE).unwrap();
        let result = part_1(&night_sky);
        let expected: &str = "\n\
            █   █  ▀█▀\n\
            █▄▄▄█   █ \n\
            █   █   █ \n\
            █   █  ▄█▄\n\
        ";
        assert_eq!(result, expected);
    }
    #[test]
    fn test_part_2() {
        let night_sky = parse(EXAMPLE).unwrap();
        let result = part_2(&night_sky);
        assert_eq!(result, 3);
    }
}
