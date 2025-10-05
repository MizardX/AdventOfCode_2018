use std::num::ParseIntError;

#[aoc_generator(day11)]
fn parse(input: &str) -> Result<u64, ParseIntError> {
    input.parse()
}

#[aoc(day11, part1)]
#[expect(clippy::trivially_copy_pass_by_ref, reason = "AOC lib")]
fn part_1(&serial: &u64) -> String {
    let mut max_fuel = i64::MIN;
    let mut max_pos = (u64::MAX, u64::MAX);
    for y in 1..299 {
        for x in 1..299 {
            let fuel = (y..y + 3)
                .map(|y1| (x..x + 3).map(|x1| fuel_level(x1, y1, serial)).sum::<i64>())
                .sum();
            if fuel > max_fuel {
                max_fuel = fuel;
                max_pos = (x, y);
            }
        }
    }
    let (x, y) = max_pos;
    format!("{x},{y}")
}

#[aoc(day11, part2)]
#[expect(clippy::trivially_copy_pass_by_ref, reason = "AOC lib")]
fn part_2(&serial: &u64) -> String {
    let mut grid = vec![0; 300 * 300];
    macro_rules! grid {
        ($x:expr,$y:expr) => {
            grid[usize::try_from((($y) - 1) * 300 + (($x) - 1)).unwrap()]
        };
    }
    for y in 1..=300 {
        for x in 1..=300 {
            let mut sum = fuel_level(x, y, serial);
            if x > 1 {
                sum += grid![x - 1, y];
                if y > 1 {
                    sum += grid![x, y - 1];
                    sum -= grid![x - 1, y - 1];
                }
            } else if y > 1 {
                sum += grid![x, y - 1];
            }
            grid![x, y] = sum;
        }
    }
    let mut max_fuel = i64::MIN;
    let mut max_pos = (u64::MAX, u64::MAX, u64::MAX);
    for y in 1..=300 {
        for x in 1..=300 {
            for s in 1..=x.min(y) {
                // square of size `s` with bottom-right coner at (`x`, `y`)
                let mut fuel = grid![x, y];
                if x > s {
                    fuel -= grid![x - s, y];
                    if y > s {
                        fuel -= grid![x, y - s];
                        fuel += grid![x - s, y - s];
                    }
                } else if y > s {
                    fuel -= grid![x, y - s];
                }
                if fuel > max_fuel {
                    max_fuel = fuel;
                    // transform into top-left corner
                    max_pos = (x - s + 1, y - s + 1, s);
                }
            }
        }
    }
    let (x, y, s) = max_pos;
    format!("{x},{y},{s}")
}

const fn fuel_level(x: u64, y: u64, serial: u64) -> i64 {
    u64::rem_euclid(((x + 10) * y + serial) * (x + 10) / 100, 10).cast_signed() - 5
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case(3, 5, 8 => 4)]
    #[test_case(122, 79, 57 => -5)]
    #[test_case(217, 196, 39 => 0)]
    #[test_case(101, 153, 71 => 4)]
    fn test_fuel_level(x: u64, y: u64, serial: u64) -> i64 {
        fuel_level(x, y, serial)
    }

    #[test_case(18 => "33,45")]
    #[test_case(42 => "21,61")]
    fn test_part_1(serial: u64) -> String {
        part_1(&serial)
    }

    #[test_case(18 => "90,269,16")]
    #[test_case(42 => "232,251,12")]
    fn test_part_2(serial: u64) -> String {
        part_2(&serial)
    }
}
