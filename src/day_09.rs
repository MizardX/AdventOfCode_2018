use std::collections::VecDeque;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MarbleGame {
    players: usize,
    last_marble: usize,
}

impl FromStr for MarbleGame {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (players, rest) = s
            .split_once(" players; last marble is worth ")
            .ok_or(ParseError::SyntaxError)?;
        let points = rest
            .strip_suffix(" points")
            .ok_or(ParseError::SyntaxError)?;
        Ok(Self {
            players: players.parse()?,
            last_marble: points.parse()?,
        })
    }
}

#[aoc_generator(day9)]
fn parse(input: &str) -> Result<MarbleGame, ParseError> {
    input.parse()
}

#[aoc(day9, part1)]
fn part_1(game: &MarbleGame) -> usize {
    play(game.players, game.last_marble)
}

#[aoc(day9, part2)]
fn part_2(game: &MarbleGame) -> usize {
    play(game.players, game.last_marble * 100)
}

fn play(players: usize, last_marble: usize) -> usize {
    let mut score = vec![0; players];
    let mut circle: VecDeque<_> = VecDeque::with_capacity(last_marble);
    circle.extend([1, 0]);
    for marble in 2..=last_marble {
        if marble.is_multiple_of(23) {
            let player = marble % players;
            score[player] += marble;
            circle.rotate_right(7);
            let extra = circle.pop_front().unwrap();
            score[player] += extra;
        } else {
            circle.rotate_left(2);
            circle.push_front(marble);
        }
    }
    score.into_iter().max().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case(9, 25 => 32)]
    #[test_case(10, 1_618 => 8_317)]
    #[test_case(13, 7_999 => 146_373)]
    #[test_case(17, 1104 => 2764)]
    #[test_case(21, 6_111 => 54_718)]
    #[test_case(30, 5_807 => 37_305)]
    fn test_play(players: usize, rounds: usize) -> usize {
        play(players, rounds)
    }
}
