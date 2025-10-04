#[aoc(day5, part1)]
fn part_1(input: &[u8]) -> usize {
    let mut stack = Vec::new();
    for &ch in input {
        if let Some(&top) = stack.last()
            && ch ^ top == 0x20
        {
            stack.pop();
        } else {
            stack.push(ch);
        }
    }
    stack.len()
}

#[aoc(day5, part2)]
fn part_2(input: &[u8]) -> usize {
    let mut stacks = [(); 26].map(|()| Vec::new());
    for &ch in input {
        for (target_ch, stack) in (b'a'..=b'z').zip(&mut stacks) {
            if ch | 0x20 == target_ch {
                continue;
            }
            if let Some(&top) = stack.last()
                && ch ^ top == 0x20
            {
                stack.pop();
            } else {
                stack.push(ch);
            }
        }
    }
    stacks.iter().map(Vec::len).min().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case(b"aA" => 0)]
    #[test_case(b"abBA" => 0)]
    #[test_case(b"abAB" => 4)]
    #[test_case(b"aabAAB" => 6)]
    #[test_case(b"dabAcCaCBAcCcaDA" => 10)]
    fn test_part_1(input: &[u8]) -> usize {
        part_1(input)
    }

    #[test_case(b"dabAcCaCBAcCcaDA" => 4)]
    fn test_part_2(input: &[u8]) -> usize {
        part_2(input)
    }
}
