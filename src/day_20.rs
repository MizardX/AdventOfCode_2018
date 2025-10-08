use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::{Debug, Display, Write};
use std::ops::{Add, Range};

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Invalid tile")]
    InvalidToken,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl TryFrom<u8> for Direction {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'N' => Self::North,
            b'E' => Self::East,
            b'S' => Self::South,
            b'W' => Self::West,
            _ => return Err(ParseError::InvalidToken),
        })
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match *self {
            Self::North => 'N',
            Self::East => 'E',
            Self::South => 'S',
            Self::West => 'W',
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Direction(Direction),
    Open,
    Alt,
    Close,
}

impl TryFrom<u8> for Token {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            b'N' | b'E' | b'S' | b'W' => Self::Direction(value.try_into()?),
            b'(' => Self::Open,
            b'|' => Self::Alt,
            b')' => Self::Close,
            _ => return Err(ParseError::InvalidToken),
        })
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match *self {
            Self::Direction(dir) => return write!(f, "{dir}"),
            Self::Open => " ( ",
            Self::Alt => " | ",
            Self::Close => " ) ",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Node {
    token: Token,
    next_alt: Option<usize>,
    prev_alt: Option<usize>,
    closing: Option<usize>,
    opening: Option<usize>,
}

impl Node {
    const fn new(token: Token) -> Self {
        Self {
            token,
            next_alt: None,
            prev_alt: None,
            closing: None,
            opening: None,
        }
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Node");
        s.field("token", &self.token);
        if let Some(next_alt) = self.next_alt {
            s.field("next_alt", &next_alt);
        }
        if let Some(prev_alt) = self.prev_alt {
            s.field("prev_alt", &prev_alt);
        }
        if let Some(closing) = self.closing {
            s.field("closing", &closing);
        }
        if let Some(opening) = self.opening {
            s.field("opening", &opening);
        }
        s.finish()
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

#[derive(Debug, Clone, Copy)]
struct NodeSlice<'a> {
    nodes: &'a [Node],
    offset: usize,
}

impl<'a> NodeSlice<'a> {
    const fn new(nodes: &'a [Node]) -> Self {
        Self { nodes, offset: 0 }
    }

    fn slice(self, range: Range<usize>) -> Self {
        let offset = self.offset + range.start;
        Self {
            nodes: &self.nodes[range],
            offset,
        }
    }
}

impl Display for NodeSlice<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for it in self.nodes {
            write!(f, "{it}")?;
        }
        Ok(())
    }
}

#[aoc_generator(day20)]
fn parse(input: &[u8]) -> Result<Vec<Token>, ParseError> {
    let n = input.len();
    input[1..n - 1].iter().map(|&b| b.try_into()).collect()
}

fn into_nodes(tokens: &[Token]) -> (Vec<Node>, Option<usize>) {
    let mut updated_nodes = tokens.iter().copied().map(Node::new).collect::<Vec<_>>();
    let mut opening = Vec::new();
    let mut alts = Vec::new();
    let mut first_alt = None;
    for (ix, &tok) in tokens.iter().enumerate() {
        match tok {
            Token::Direction(..) => {}
            Token::Open => {
                opening.push(ix);
                alts.push(ix);
            }
            Token::Alt => {
                if let Some(prev) = alts.pop() {
                    updated_nodes[prev].next_alt = Some(ix);
                    updated_nodes[ix].prev_alt = Some(prev);
                } else {
                    first_alt = Some(ix);
                }
                alts.push(ix);
            }
            Token::Close => {
                let open = opening.pop().unwrap();
                updated_nodes[open].closing = Some(ix);
                updated_nodes[ix].opening = Some(open);
                if let Some(prev) = alts.pop() {
                    updated_nodes[ix].prev_alt = Some(prev);
                }
            }
        }
    }
    (updated_nodes, first_alt)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Position {
    x: i32,
    y: i32,
}

impl Add<Direction> for Position {
    type Output = Self;

    fn add(mut self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::North => self.y -= 1,
            Direction::East => self.x += 1,
            Direction::South => self.y += 1,
            Direction::West => self.x -= 1,
        }
        self
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("").field(&self.x).field(&self.y).finish()
    }
}

#[aoc(day20, part1)]
fn part_1(tokens: &[Token]) -> usize {
    let (nodes, first_alt) = into_nodes(tokens);
    let mut visited = HashMap::new();
    walk_alt(
        Position::default(),
        NodeSlice::new(&nodes),
        first_alt,
        0,
        &mut visited,
    );
    visited.values().copied().max().unwrap()
}

#[aoc(day20, part2)]
fn part_2(tokens: &[Token]) -> usize {
    count_paths(tokens, 1_000)
}

fn count_paths(tokens: &[Token], min_len: usize) -> usize {
    let (nodes, first_alt) = into_nodes(tokens);
    let mut visited = HashMap::new();
    walk_alt(
        Position::default(),
        NodeSlice::new(&nodes),
        first_alt,
        0,
        &mut visited,
    );
    visited.values().filter(|&&v| v >= min_len).count()
}

fn walk_alt(
    position: Position,
    nodes: NodeSlice,
    first_alt: Option<usize>,
    accum_len: usize,
    visited: &mut HashMap<Position, usize>,
) -> usize {
    let mut max_len = 0;
    if let Some(alt) = first_alt {
        let alt_len = walk_sequence(
            position,
            nodes.slice(0..alt - nodes.offset),
            accum_len,
            visited,
        );
        max_len = max_len.max(alt_len);
        let mut ix = alt - nodes.offset;
        while let Some(alt) = nodes.nodes[ix].next_alt {
            let alt_len = walk_sequence(
                position,
                nodes.slice(ix + 1..alt - nodes.offset),
                accum_len,
                visited,
            );
            max_len = max_len.max(alt_len);
            ix = alt - nodes.offset;
        }
        let alt_len = walk_sequence(
            position,
            nodes.slice(ix + 1..nodes.nodes.len()),
            accum_len,
            visited,
        );
        max_len = max_len.max(alt_len);
        max_len
    } else {
        walk_sequence(position, nodes, accum_len, visited)
    }
}

fn walk_sequence(
    mut position: Position,
    nodes: NodeSlice,
    accum_len: usize,
    visited: &mut HashMap<Position, usize>,
) -> usize {
    let mut ix = 0;
    let mut len = 0;
    while ix < nodes.nodes.len() {
        match nodes.nodes[ix].token {
            Token::Direction(dir) => {
                position = position + dir;
                len += 1;
                match visited.entry(position) {
                    Entry::Occupied(o) => len = *o.get() - accum_len,
                    Entry::Vacant(v) => {
                        v.insert(accum_len + len);
                    }
                }
            }
            Token::Open => {
                let close = nodes.nodes[ix].closing.unwrap() - nodes.offset;
                let group_len = walk_alt(
                    position,
                    nodes.slice(ix + 1..close),
                    nodes.nodes[ix].next_alt,
                    accum_len + len,
                    visited,
                );
                len += group_len;
                ix = close;
            }
            Token::Alt => unreachable!("Should not be any alts here."),
            Token::Close => unreachable!("Should not be any closing here."),
        }
        ix += 1;
    }
    len
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    const EXAMPLE1: &[u8] = b"^WNE$";
    const EXAMPLE2: &[u8] = b"^ENWWW(NEEE|SSE(EE|N))$";
    const EXAMPLE3: &[u8] = b"^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$";
    const EXAMPLE4: &[u8] = b"^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$";
    const EXAMPLE5: &[u8] = b"^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE2).unwrap();
        assert_eq!(
            result,
            [
                Token::Direction(Direction::East),
                Token::Direction(Direction::North),
                Token::Direction(Direction::West),
                Token::Direction(Direction::West),
                Token::Direction(Direction::West),
                Token::Open,
                Token::Direction(Direction::North),
                Token::Direction(Direction::East),
                Token::Direction(Direction::East),
                Token::Direction(Direction::East),
                Token::Alt,
                Token::Direction(Direction::South),
                Token::Direction(Direction::South),
                Token::Direction(Direction::East),
                Token::Open,
                Token::Direction(Direction::East),
                Token::Direction(Direction::East),
                Token::Alt,
                Token::Direction(Direction::North),
                Token::Close,
                Token::Close
            ]
        );
    }

    #[test_case(EXAMPLE1 => 3)]
    #[test_case(EXAMPLE2 => 10)]
    #[test_case(EXAMPLE3 => 18)]
    #[test_case(EXAMPLE4 => 23)]
    #[test_case(EXAMPLE5 => 31)]
    fn test_part_1(input: &[u8]) -> usize {
        let tokens = parse(input).unwrap();
        part_1(&tokens)
    }

    #[test_case(EXAMPLE1, 0 => 4)]
    #[test_case(EXAMPLE1, 4 => 1)]
    #[test_case(EXAMPLE2, 8 => 6)]
    fn test_part_2(input: &[u8], min_len: usize) -> usize {
        let tokens = parse(input).unwrap();
        count_paths(&tokens, min_len)
    }
}
