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
struct Point {
    x: i32,
    y: i32,
    z: i32,
    t: i32,
}

impl Point {
    const fn dist(self, other: Self) -> u32 {
        self.x.abs_diff(other.x)
            + self.y.abs_diff(other.y)
            + self.z.abs_diff(other.z)
            + self.t.abs_diff(other.t)
    }
}

impl FromStr for Point {
    type Err = ParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.split(',').map(str::trim_ascii).map(str::parse);
        let x = parts.next().ok_or(ParseError::SyntaxError)??;
        let y = parts.next().ok_or(ParseError::SyntaxError)??;
        let z = parts.next().ok_or(ParseError::SyntaxError)??;
        let t = parts.next().ok_or(ParseError::SyntaxError)??;
        Ok(Self { x, y, z, t })
    }
}

#[aoc_generator(day25)]
fn parse(input: &str) -> Result<Vec<Point>, ParseError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day25, part1)]
fn part_1(points: &[Point]) -> usize {
    let mut uf = UnionFind::new(points.len());
    for (i, &p1) in points.iter().enumerate() {
        for (j, &p2) in points[..i].iter().enumerate() {
            if p1.dist(p2) <= 3 {
                uf.union(j, i);
            }
        }
    }
    uf.size
}

#[derive(Debug, Clone, Copy)]
struct UnionFindNode {
    parent: usize,
    size: usize,
}

#[derive(Debug, Clone)]
struct UnionFind {
    nodes: Vec<UnionFindNode>,
    size: usize,
}

impl UnionFind {
    fn new(size: usize) -> Self {
        Self {
            nodes: (0..size)
                .map(|parent| UnionFindNode { parent, size: 1 })
                .collect(),
            size,
        }
    }

    fn find(&mut self, mut index: usize) -> usize {
        let mut parent = self.nodes[index].parent;
        while index != parent {
            let grand_parent = self.nodes[parent].parent;
            self.nodes[index].parent = grand_parent;
            index = parent;
            parent = grand_parent;
        }
        index
    }

    fn union(&mut self, mut index1: usize, mut index2: usize) -> bool {
        index1 = self.find(index1);
        index2 = self.find(index2);
        if index1 == index2 {
            return false;
        }
        if self.nodes[index1].size < self.nodes[index2].size {
            (index1, index2) = (index2, index1);
        }

        self.nodes[index2].parent = index1;
        self.nodes[index1].size += self.nodes[index2].size;
        self.size -= 1;
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    const EXAMPLE1: &str = " \
         0,0,0,0\n \
         3,0,0,0\n \
         0,3,0,0\n \
         0,0,3,0\n \
         0,0,0,3\n \
         0,0,0,6\n \
         9,0,0,0\n\
        12,0,0,0\
    ";

    const EXAMPLE2: &str = "\
        -1,2,2,0\n\
        0,0,2,-2\n\
        0,0,0,-2\n\
        -1,2,0,0\n\
        -2,-2,-2,2\n\
        3,0,2,-1\n\
        -1,3,2,2\n\
        -1,0,-1,0\n\
        0,2,1,-2\n\
        3,0,0,0\
    ";

    const EXAMPLE3: &str = "\
        1,-1,0,1\n\
        2,0,-1,0\n\
        3,2,-1,0\n\
        0,0,3,1\n\
        0,0,-1,-1\n\
        2,3,-2,0\n\
        -2,2,0,0\n\
        2,-2,0,-1\n\
        1,-1,0,-1\n\
        3,2,0,2\
    ";

    const EXAMPLE4: &str = "\
        1,-1,-1,-2\n\
        -2,-2,0,1\n\
        0,2,1,3\n\
        -2,3,-2,1\n\
        0,2,3,-2\n\
        -1,-1,1,-2\n\
        0,-2,-1,0\n\
        -2,2,3,-1\n\
        1,2,2,0\n\
        -1,-2,0,-2\
    ";

    #[test_case(EXAMPLE1 => 2)]
    #[test_case(EXAMPLE2 => 4)]
    #[test_case(EXAMPLE3 => 3)]
    #[test_case(EXAMPLE4 => 8)]
    fn test_part_1(input: &str) -> usize {
        let points = parse(input).unwrap();
        part_1(&points)
    }
}
