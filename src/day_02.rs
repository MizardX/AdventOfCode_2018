use std::collections::HashSet;

#[aoc(day2, part1)]
fn part_1(input: &str) -> usize {
    let mut twos = 0;
    let mut threes = 0;
    for line in input.lines() {
        let mut counts = [0; 26];
        for ch in line.bytes() {
            counts[(ch - b'a') as usize] += 1;
        }
        let mut has_two = false;
        let mut has_three = false;
        for count in counts {
            if count == 2 {
                has_two = true;
            }
            if count == 3 {
                has_three = true;
            }
        }
        twos += usize::from(has_two);
        threes += usize::from(has_three);
    }
    twos * threes
}

#[aoc(day2, part2)]
fn part_2(input: &str) -> String {
    let mut seen = HashSet::new();
    for id in input.lines() {
        for cut in 0..id.len() {
            if !seen.insert((&id[..cut], &id[cut + 1..])) {
                let mut result = String::with_capacity(id.len() - 1);
                result.push_str(&id[..cut]);
                result.push_str(&id[cut + 1..]);
                return result;
            }
        }
    }
    String::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &str = "\
        abcdef\n\
        bababc\n\
        abbcde\n\
        abcccd\n\
        aabcdd\n\
        abcdee\n\
        ababab\
    ";

    const EXAMPLE2: &str = "\
        abcde\n\
        fghij\n\
        klmno\n\
        pqrst\n\
        fguij\n\
        axcye\n\
        wvxyz\
    ";

    #[test]
    fn test_part_1() {
        let result = part_1(EXAMPLE1);
        assert_eq!(result, 12);
    }

    #[test]
    fn test_part_2() {
        let result = part_2(EXAMPLE2);
        assert_eq!(result, "fgij");
    }
}
