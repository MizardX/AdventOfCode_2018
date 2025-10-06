use std::{collections::VecDeque, fmt::Display};

#[aoc(day14, part1)]
fn part_1(steps: &str) -> String {
    let steps: usize = steps.parse().expect("An integer");
    let mut chocolate_charts = ChocolateCharts::new();
    for _ in 0..steps {
        chocolate_charts.next();
    }
    let result: Vec<u8> = chocolate_charts.take(10).collect();
    unsafe { String::from_utf8_unchecked(result) }
}

#[aoc(day14, part2)]
fn part_2(target_str: &str) -> usize {
    let target = target_str.as_bytes();
    let mut tail = VecDeque::new();
    for (index, ch) in ChocolateCharts::new().take(100_000_000).enumerate() {
        tail.push_back(ch);
        if tail.len() > target.len() {
            tail.pop_front();
        }
        if tail == target {
            return index + 1 - target.len();
        }
    }
    0
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Turn {
    A,
    B,
}

#[derive(Debug, Clone)]
struct ChocolateCharts {
    scores: Vec<u8>,
    a: usize,
    b: usize,
    turn: Turn,
}

impl ChocolateCharts {
    const fn new() -> Self {
        Self {
            scores: Vec::new(),
            a: 0,
            b: 0,
            turn: Turn::A,
        }
    }
}

impl Iterator for ChocolateCharts {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match self.scores.len() {
            0 => {
                self.scores.push(b'3');
                self.a = 0;
                return Some(b'3');
            }
            1 => {
                self.scores.push(b'7');
                self.b = 1;
                self.turn = Turn::A;
                return Some(b'7');
            }
            _ => {}
        }
        loop {
            let score_a = self.scores[self.a] - b'0';
            let score_b = self.scores[self.b] - b'0';
            let sum = score_a + score_b;
            let new_score_a = sum / 10;
            let new_score_b = sum % 10;
            match self.turn {
                Turn::A => {
                    self.turn = Turn::B;
                    if new_score_a > 0 {
                        self.scores.push(b'0' + new_score_a);
                        return Some(b'0' + new_score_a);
                    }
                }
                Turn::B => {
                    self.turn = Turn::A;
                    self.scores.push(b'0' + new_score_b);
                    self.a = (self.a + score_a as usize + 1) % self.scores.len();
                    self.b = (self.b + score_b as usize + 1) % self.scores.len();
                    return Some(b'0' + new_score_b);
                }
            }
        }
    }
}

impl Display for ChocolateCharts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (a, b) = (self.a, self.b);
        if self.scores.is_empty() {
            write!(f, "EMPTY")?;
        } else if a < b {
            for &x in &self.scores[0..a] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
            write!(f, "({})", self.scores[a] as char)?;
            for &x in &self.scores[a + 1..b] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
            write!(f, "[{}]", self.scores[b] as char)?;
            for &x in &self.scores[b + 1..self.scores.len()] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
        } else if a > b {
            for &x in &self.scores[0..b] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
            write!(f, "[{}]", self.scores[b] as char)?;
            for &x in &self.scores[b + 1..a] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
            write!(f, "({})", self.scores[a] as char)?;
            for &x in &self.scores[a + 1..self.scores.len()] {
                let x = x as char;
                write!(f, " {x} ")?;
            }
        } else {
            for &x in &self.scores {
                let x = x as char;
                write!(f, " {x} ")?;
            }
        }
        writeln!(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case("9" => "5158916779")]
    #[test_case("5" => "0124515891")]
    #[test_case("18" => "9251071085")]
    #[test_case("2018" => "5941429882")]
    fn test_part_1(steps: &str) -> String {
        part_1(steps)
    }

    #[test_case("51589" => 9)]
    #[test_case("01245" => 5)]
    #[test_case("92510" => 18)]
    #[test_case("59414" => 2018)]
    fn test_part_2(target: &str) -> usize {
        part_2(target)
    }
}
