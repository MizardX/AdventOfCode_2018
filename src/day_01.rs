use std::collections::HashSet;
use std::num::ParseIntError;

#[aoc_generator(day1)]
fn parse(input: &str) -> Result<Vec<i32>, ParseIntError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day1, part1)]
fn part_1(drifts: &[i32]) -> i32 {
    drifts.iter().copied().sum()
}

#[aoc(day1, part2)]
fn part_2(drifts: &[i32]) -> i32 {
    let mut seen: HashSet<_> = [0].into();
    let mut freq = 0;
    drifts
        .iter()
        .copied()
        .cycle()
        .find_map(|drift| {
            freq += drift;
            (!seen.insert(freq)).then_some(freq)
        })
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn test_parse() {
        let result = parse("+1\n-2\n+3\n+1").unwrap();
        assert_eq!(result, [1, -2, 3, 1]);
    }

    #[test_case(&[1,-2,3,1] => 3)]
    #[test_case(&[1,1,1] => 3)]
    #[test_case(&[1,1,-2] => 0)]
    #[test_case(&[-1,-2,-3] => -6)]
    fn test_part_1(drifts: &[i32]) -> i32 {
        part_1(drifts)
    }

    #[test_case(&[1,-2,3,1] => 2)]
    #[test_case(&[1,-1] => 0)]
    #[test_case(&[3,3,4,-2,-4] => 10)]
    #[test_case(&[-6,3,8,5,-6] => 5)]
    #[test_case(&[7,7,-2,-7,-4] => 14)]
    fn test_part_2(drifts: &[i32]) -> i32 {
        part_2(drifts)
    }
}
