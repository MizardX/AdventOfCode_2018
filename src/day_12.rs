use std::collections::HashMap;
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Syntax error")]
    SyntaxError,
    #[error("Not a pot")]
    InvalidPot,
    #[error("Rule malformed")]
    InvalidRule,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pot {
    Empty,
    Plant,
}

impl TryFrom<u8> for Pot {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'.' => Self::Empty,
            b'#' => Self::Plant,
            _ => return Err(ParseError::InvalidPot),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Rule {
    pattern: [Pot; 5],
    result: Pot,
}

const fn mode_index([p1, p2, p3, p4, p5]: [Pot; 5]) -> usize {
    (p1 as usize) << 4
        | (p2 as usize) << 3
        | (p3 as usize) << 2
        | (p4 as usize) << 1
        | (p5 as usize)
}

impl FromStr for Rule {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 5 + 4 + 1 || &s[5..5 + 4] != " => " {
            return Err(ParseError::InvalidRule);
        }
        let mut pattern = [Pot::Empty; 5];
        for (i, ch) in s[0..5].bytes().enumerate() {
            pattern[i] = ch.try_into()?;
        }
        let result = s.as_bytes()[9].try_into()?;
        Ok(Self { pattern, result })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Farm {
    initial_state: Vec<Pot>,
    spread: Vec<Rule>,
}

impl Farm {
    const fn mode(&self) -> [Pot; 1 << 5] {
        let mut mode = [Pot::Empty; 1 << 5];
        let spread = self.spread.as_slice();
        let mut ix = 0;
        let len = spread.len();
        while ix < len {
            let rule = &spread[ix];
            mode[mode_index(rule.pattern)] = rule.result;
            ix += 1;
        }
        mode
    }
}

impl FromStr for Farm {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let initial_state = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("initial state: ")
            .ok_or(ParseError::SyntaxError)?
            .bytes()
            .map(TryInto::try_into)
            .collect::<Result<_, _>>()?;
        if lines.next() != Some("") {
            return Err(ParseError::SyntaxError);
        }
        let spread = lines.map(str::parse).collect::<Result<_, _>>()?;
        Ok(Self {
            initial_state,
            spread,
        })
    }
}

#[aoc_generator(day12)]
fn parse(input: &str) -> Result<Farm, ParseError> {
    input.parse()
}

#[aoc(day12, part1)]
fn part_1(farm: &Farm) -> isize {
    simulate_farm(farm, 20)
}

#[aoc(day12, part2)]
fn part_2(farm: &Farm) -> isize {
    let target = 50_000_000_000;
    let mut prev = 0;
    let mut seen = HashMap::new();
    let mut rolling_key = [0; 5];
    for (time, plants) in (1..).zip(FarmSimulation::new(farm)) {
        let delta = plants - prev;
        prev = plants;
        rolling_key[0] = delta;
        rolling_key.rotate_left(1);
        if time < rolling_key.len().cast_signed() {
            continue;
        }
        if let Some(&old_time) = seen.get(&rolling_key) {
            let cycle_len: isize = time - old_time;
            let remaining_cycles = (target - time) / cycle_len;
            let cycle_value = rolling_key[..cycle_len.cast_unsigned()]
                .iter()
                .copied()
                .sum::<isize>();
            // NOTE: If the `cycle_len` was not size 1, and the remaining iterations was not
            // evenly divisible by it, we would have to go a few steps further after
            // `remaining_cycles`*`cycle_len` to reach the target.
            // This should be covered by the deltas in `rolling_key`, but I have not tried to
            // solve that part.
            return plants + remaining_cycles * cycle_value;
        }
        seen.insert(rolling_key, time);
    }
    0
}

fn simulate_farm(farm: &Farm, time: usize) -> isize {
    FarmSimulation::new(farm).nth(time - 1).unwrap()
}

fn simulate_step(current: &[Pot], next: &mut Vec<Pot>, mode: &[Pot; 32]) {
    let mut rolling_key = [Pot::Empty; 5];
    for &cur in current {
        rolling_key[0] = cur;
        rolling_key.rotate_left(1);
        let mode_index = mode_index(rolling_key);
        next.push(mode[mode_index]);
    }
    for _ in 0..4 {
        rolling_key[0] = Pot::Empty;
        rolling_key.rotate_left(1);
        let mode_index = mode_index(rolling_key);
        next.push(mode[mode_index]);
    }
}

#[allow(unused)]
fn display_state(state: &[Pot]) -> String {
    unsafe {
        String::from_utf8_unchecked(
            state
                .iter()
                .map(|&p| match p {
                    Pot::Empty => b'.',
                    Pot::Plant => b'#',
                })
                .collect::<Vec<u8>>(),
        )
    }
}

struct FarmSimulation {
    mode: [Pot; 32],
    current: Vec<Pot>,
    next: Vec<Pot>,
    index_of_pot_zero: isize,
}

impl FarmSimulation {
    fn new(farm: &Farm) -> Self {
        let mode = farm.mode();
        let current = farm.initial_state.clone();
        let next = Vec::with_capacity(current.capacity());
        Self {
            mode,
            current,
            next,
            index_of_pot_zero: 0,
        }
    }
}

impl Iterator for FarmSimulation {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        simulate_step(&self.current, &mut self.next, &self.mode);
        let first = self.next.iter().position(|&p| p == Pot::Plant).unwrap();
        let last = self.next.iter().rposition(|&p| p == Pot::Plant).unwrap();
        self.index_of_pot_zero += 2 - first.cast_signed();
        self.next.rotate_left(first);
        self.next.truncate(last - first + 1);
        std::mem::swap(&mut self.current, &mut self.next);
        self.next.clear();

        Some(
            self.current
                .iter()
                .enumerate()
                .map(|(ix, &p)| p as isize * (ix.cast_signed() - self.index_of_pot_zero))
                .sum(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        initial state: #..#.#..##......###...###\n\
        \n\
        ...## => #\n\
        ..#.. => #\n\
        .#... => #\n\
        .#.#. => #\n\
        .#.## => #\n\
        .##.. => #\n\
        .#### => #\n\
        #.#.# => #\n\
        #.### => #\n\
        ##.#. => #\n\
        ##.## => #\n\
        ###.. => #\n\
        ###.# => #\n\
        ####. => #\
    ";

    #[test]
    fn test_parse() {
        const E: Pot = Pot::Empty;
        const P: Pot = Pot::Plant;
        const fn rule(pattern: [Pot; 5], result: Pot) -> Rule {
            Rule { pattern, result }
        }
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result.initial_state,
            [
                P, E, E, P, E, P, E, E, P, P, E, E, E, E, E, E, P, P, P, E, E, E, P, P, P
            ]
        );
        assert_eq!(
            result.spread,
            [
                rule([E, E, E, P, P], P),
                rule([E, E, P, E, E], P),
                rule([E, P, E, E, E], P),
                rule([E, P, E, P, E], P),
                rule([E, P, E, P, P], P),
                rule([E, P, P, E, E], P),
                rule([E, P, P, P, P], P),
                rule([P, E, P, E, P], P),
                rule([P, E, P, P, P], P),
                rule([P, P, E, P, E], P),
                rule([P, P, E, P, P], P),
                rule([P, P, P, E, E], P),
                rule([P, P, P, E, P], P),
                rule([P, P, P, P, E], P),
            ]
        );
    }

    #[test]
    fn test_mode() {
        const E: Pot = Pot::Empty;
        const P: Pot = Pot::Plant;
        let result = parse(EXAMPLE).unwrap();
        let mode = result.mode();
        assert_eq!(
            mode,
            [
                E, E, E, P, P, E, E, E, P, E, P, P, P, E, E, P, E, E, E, E, E, P, E, P, E, E, P, P,
                P, P, P, E
            ]
        );
    }

    #[test]
    fn test_mode_apply() {
        let mut state = [Pot::Empty; 32 * 5];
        for i in 0..32 {
            for j in 0..5 {
                state[5 * i + j] = [Pot::Empty, Pot::Plant][(i >> (4 - j)) & 1];
            }
        }
        let mut next = Vec::with_capacity(32 * 5 + 4);
        for i in 0..32 {
            let mut mode = [Pot::Empty; 32];
            mode[i] = Pot::Plant;
            simulate_step(&state, &mut next, &mode);
            let target = (4..32 * 5 + 4)
                .step_by(5)
                .map(|ix| next[ix])
                .collect::<Vec<_>>();
            assert_eq!(display_state(&target), display_state(&mode));
            next.clear();
        }
    }

    #[test]
    fn test_part_1() {
        let farm = parse(EXAMPLE).unwrap();
        let result = part_1(&farm);
        assert_eq!(result, 325);
    }

    #[test]
    fn test_part_2() {
        let farm = parse(EXAMPLE).unwrap();
        let result = part_2(&farm);
        assert_eq!(result, 999_999_999_374);
    }
}
