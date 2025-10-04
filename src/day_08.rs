use std::num::ParseIntError;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Not enough data to form a node")]
    NotEnoughData,
    #[error("Trailing data at the end could not to parsed into a node")]
    TrailingGarbage,
    #[error(transparent)]
    InvalidNumber(#[from] ParseIntError),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<u8>,
    size: usize,
}

impl Node {
    fn get_value(&self) -> u32 {
        if self.children.is_empty() {
            self.metadata.iter().map(|&x| u32::from(x)).sum()
        } else {
            let n = self.children.len();
            let mut child_cache = vec![None; n];
            let mut sum = 0;
            for &meta in &self.metadata {
                if let Some(ix) = meta.checked_sub(1).map(usize::from)
                    && ix < n
                {
                    sum += *child_cache[ix].get_or_insert_with(|| self.children[ix].get_value());
                }
            }
            sum
        }
    }
}

impl TryFrom<&[u8]> for Node {
    type Error = ParseError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if let &[num_children, metadata_size, ..] = value {
            let mut node = Self::default();
            let mut rest = &value[2..];
            node.size = 2;
            for _ in 0..num_children {
                let child: Self = rest.try_into()?;
                rest = rest.get(child.size..).ok_or(ParseError::NotEnoughData)?;
                node.size += child.size;
                node.children.push(child);
            }
            node.metadata = rest
                .get(..metadata_size as usize)
                .ok_or(ParseError::NotEnoughData)?
                .to_vec();
            node.size += metadata_size as usize;
            Ok(node)
        } else {
            Err(ParseError::NotEnoughData)
        }
    }
}

#[aoc_generator(day8)]
fn parse(input: &str) -> Result<Node, ParseError> {
    let nums: Vec<u8> = input
        .split_ascii_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    let root: Node = nums.as_slice().try_into()?;
    if root.size != nums.len() {
        return Err(ParseError::TrailingGarbage);
    }
    Ok(root)
}

#[aoc(day8, part1)]
fn part_1(node: &Node) -> u32 {
    let mut sum = node.metadata.iter().map(|&x| u32::from(x)).sum::<u32>();
    for child in &node.children {
        sum += part_1(child);
    }
    sum
}

#[aoc(day8, part2)]
fn part_2(node: &Node) -> u32 {
    node.get_value()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            Node {
                children: vec![
                    Node {
                        children: vec![],
                        metadata: vec![10, 11, 12],
                        size: 5,
                    },
                    Node {
                        children: vec![Node {
                            children: vec![],
                            metadata: vec![99],
                            size: 3,
                        }],
                        metadata: vec![2],
                        size: 6
                    }
                ],
                metadata: vec![1, 1, 2],
                size: 16
            }
        );
    }

    #[test]
    fn test_part_1() {
        let node = parse(EXAMPLE).unwrap();
        let result = part_1(&node);
        assert_eq!(result, 138);
    }

    #[test]
    fn test_part_2() {
        let node = parse(EXAMPLE).unwrap();
        let result = part_2(&node);
        assert_eq!(result, 66);
    }
}
