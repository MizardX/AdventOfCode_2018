use std::collections::HashMap;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Time {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

impl Time {
    #[cfg(test)]
    pub const fn new(year: u16, month: u8, day: u8, hour: u8, minute: u8) -> Self {
        Self {
            year,
            month,
            day,
            hour,
            minute,
        }
    }
}

impl FromStr for Time {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            year: s[0..4].parse()?,
            month: s[5..7].parse()?,
            day: s[8..10].parse()?,
            hour: s[11..13].parse()?,
            minute: s[14..16].parse()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Event {
    BeginsShift(usize),
    FallsAsleep,
    WakesUp,
}

impl FromStr for Event {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "falls asleep" => Self::FallsAsleep,
            "wakes up" => Self::WakesUp,
            _ => Self::BeginsShift(
                s.strip_prefix("Guard #")
                    .ok_or(ParseError::SyntaxError)?
                    .strip_suffix(" begins shift")
                    .ok_or(ParseError::SyntaxError)?
                    .parse()?,
            ),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Log {
    time: Time,
    event: Event,
}

impl Log {
    #[cfg(test)]
    const fn new(time: Time, event: Event) -> Self {
        Self { time, event }
    }
}

impl FromStr for Log {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (time, event) = s
            .strip_prefix("[")
            .ok_or(ParseError::SyntaxError)?
            .split_once("] ")
            .ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            time: time.parse()?,
            event: event.parse()?,
        })
    }
}

#[aoc_generator(day4)]
fn parse(input: &str) -> Result<Vec<Log>, ParseError> {
    let mut result: Vec<Log> = input.lines().map(str::parse).collect::<Result<_, _>>()?;
    result.sort_unstable_by_key(|l| l.time);
    Ok(result)
}

#[derive(Debug, Clone)]
struct Guard {
    id: usize,
    minutes_asleep: usize,
    frequency: [u16; 60],
}

impl Guard {
    const fn new(id: usize) -> Self {
        Self {
            id,
            minutes_asleep: 0,
            frequency: [0; 60],
        }
    }

    fn register_asleep(&mut self, start: u8, end: u8) {
        self.minutes_asleep += usize::from(end.checked_sub(start).expect("positive minutes"));
        for m in start..end {
            self.frequency[m as usize] += 1;
        }
    }
}

fn calculate_statistics(logs: &[Log]) -> Vec<Guard> {
    let mut guard_lookup = HashMap::new();
    let mut guards = Vec::new();
    let mut current_guard = 0;
    let mut fell_asleep = None;
    for log in logs {
        match log.event {
            Event::BeginsShift(id) => {
                current_guard = *guard_lookup.entry(id).or_insert_with(|| {
                    let n = guards.len();
                    guards.push(Guard::new(id));
                    n
                });
            }
            Event::FallsAsleep => {
                fell_asleep = Some(log.time);
            }
            Event::WakesUp => {
                let start = fell_asleep.expect("event order");
                guards[current_guard].register_asleep(start.minute, log.time.minute);
                fell_asleep = None;
            }
        }
    }
    guards
}

#[aoc(day4, part1)]
fn part_1(logs: &[Log]) -> usize {
    let guards = calculate_statistics(logs);
    let sleepiest_guard = guards.iter().max_by_key(|g| g.minutes_asleep).unwrap();
    let sleepiest_minute = sleepiest_guard.frequency.iter().zip(0..).max().unwrap().1;
    sleepiest_guard.id * sleepiest_minute
}

#[aoc(day4, part2)]
fn part_2(logs: &[Log]) -> usize {
    let guards = calculate_statistics(logs);
    let (_, id, minute) = guards
        .iter()
        .map(|g| {
            let (freq, minute) = g.frequency.iter().copied().zip(0..).max().unwrap();
            (freq, g.id, minute)
        })
        .max()
        .unwrap();
    id * minute
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        [1518-11-01 00:00] Guard #10 begins shift\n\
        [1518-11-01 00:05] falls asleep\n\
        [1518-11-01 00:25] wakes up\n\
        [1518-11-01 00:30] falls asleep\n\
        [1518-11-01 00:55] wakes up\n\
        [1518-11-01 23:58] Guard #99 begins shift\n\
        [1518-11-02 00:40] falls asleep\n\
        [1518-11-02 00:50] wakes up\n\
        [1518-11-03 00:05] Guard #10 begins shift\n\
        [1518-11-03 00:24] falls asleep\n\
        [1518-11-03 00:29] wakes up\n\
        [1518-11-04 00:02] Guard #99 begins shift\n\
        [1518-11-04 00:36] falls asleep\n\
        [1518-11-04 00:46] wakes up\n\
        [1518-11-05 00:03] Guard #99 begins shift\n\
        [1518-11-05 00:45] falls asleep\n\
        [1518-11-05 00:55] wakes up\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            [
                Log::new(Time::new(1518, 11, 1, 0, 0), Event::BeginsShift(10)),
                Log::new(Time::new(1518, 11, 1, 0, 5), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 1, 0, 25), Event::WakesUp),
                Log::new(Time::new(1518, 11, 1, 0, 30), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 1, 0, 55), Event::WakesUp),
                Log::new(Time::new(1518, 11, 1, 23, 58), Event::BeginsShift(99)),
                Log::new(Time::new(1518, 11, 2, 0, 40), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 2, 0, 50), Event::WakesUp),
                Log::new(Time::new(1518, 11, 3, 0, 5), Event::BeginsShift(10)),
                Log::new(Time::new(1518, 11, 3, 0, 24), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 3, 0, 29), Event::WakesUp),
                Log::new(Time::new(1518, 11, 4, 0, 2), Event::BeginsShift(99)),
                Log::new(Time::new(1518, 11, 4, 0, 36), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 4, 0, 46), Event::WakesUp),
                Log::new(Time::new(1518, 11, 5, 0, 3), Event::BeginsShift(99)),
                Log::new(Time::new(1518, 11, 5, 0, 45), Event::FallsAsleep),
                Log::new(Time::new(1518, 11, 5, 0, 55), Event::WakesUp),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let logs = parse(EXAMPLE).unwrap();
        let result = part_1(&logs);
        assert_eq!(result, 240);
    }

    #[test]
    fn test_part_2() {
        let logs = parse(EXAMPLE).unwrap();
        let result = part_2(&logs);
        assert_eq!(result, 4455);
    }
}
