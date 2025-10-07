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
enum Opcode {
    /// add register
    AddR,
    /// add immediate
    AddI,
    /// multiply register
    MulR,
    /// multiply immediate
    MulI,
    /// bitwise AND register
    BAnR,
    /// bitwise AND immediate
    BAnI,
    /// bitwise OR register
    BOrR,
    /// bitwise OR immediate
    BOrI,
    /// set register
    SetR,
    /// set immediate
    SetI,
    /// greater-than immediate/register
    GtIR,
    /// greater-than register/immediate
    GtRI,
    /// greater-than register/register
    GtRR,
    /// equal immediate/register
    EqIR,
    /// equal register/immediate
    EqRI,
    /// equal register/register
    EqRR,
}

impl Opcode {
    const fn all() -> [Self; 16] {
        [
            Self::AddR,
            Self::AddI,
            Self::MulR,
            Self::MulI,
            Self::BAnR,
            Self::BAnI,
            Self::BOrR,
            Self::BOrI,
            Self::SetR,
            Self::SetI,
            Self::GtIR,
            Self::GtRI,
            Self::GtRR,
            Self::EqIR,
            Self::EqRI,
            Self::EqRR,
        ]
    }

    const fn execute(self, a: u8, b: u8, c: u8, registers: &mut [u32; 4]) {
        registers[c as usize] = match self {
            Self::AddR => registers[a as usize] + registers[b as usize],
            Self::AddI => registers[a as usize] + b as u32,
            Self::MulR => registers[a as usize] * registers[b as usize],
            Self::MulI => registers[a as usize] * b as u32,
            Self::BAnR => registers[a as usize] & registers[b as usize],
            Self::BAnI => registers[a as usize] & b as u32,
            Self::BOrR => registers[a as usize] | registers[b as usize],
            Self::BOrI => registers[a as usize] | b as u32,
            Self::SetR => registers[a as usize],
            Self::SetI => a as u32,
            Self::GtIR => (a as u32 > registers[b as usize]) as u32,
            Self::GtRI => (registers[a as usize] > b as u32) as u32,
            Self::GtRR => (registers[a as usize] > registers[b as usize]) as u32,
            Self::EqIR => (a as u32 == registers[b as usize]) as u32,
            Self::EqRI => (registers[a as usize] == b as u32) as u32,
            Self::EqRR => (registers[a as usize] == registers[b as usize]) as u32,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Instruction {
    op: u8,
    a: u8,
    b: u8,
    c: u8,
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split_ascii_whitespace();
        let op = words.next().ok_or(ParseError::SyntaxError)?.parse()?;
        let a = words.next().ok_or(ParseError::SyntaxError)?.parse()?;
        let b = words.next().ok_or(ParseError::SyntaxError)?.parse()?;
        let c = words.next().ok_or(ParseError::SyntaxError)?.parse()?;
        if words.next().is_some() {
            return Err(ParseError::SyntaxError);
        }
        Ok(Self { op, a, b, c })
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TestCase {
    before: [u32; 4],
    instruction: Instruction,
    after: [u32; 4],
}

impl FromStr for TestCase {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let before = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("Before: [")
            .ok_or(ParseError::SyntaxError)?
            .strip_suffix("]")
            .ok_or(ParseError::SyntaxError)?
            .splitn(4, ", ")
            .map(str::parse)
            .collect::<Result<Vec<_>, ParseIntError>>()?
            .try_into()
            .map_err(|_| ParseError::SyntaxError)?;
        let instruction = lines.next().ok_or(ParseError::SyntaxError)?.parse()?;
        let after = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("After:  [")
            .ok_or(ParseError::SyntaxError)?
            .strip_suffix("]")
            .ok_or(ParseError::SyntaxError)?
            .splitn(4, ", ")
            .map(str::parse)
            .collect::<Result<Vec<_>, ParseIntError>>()?
            .try_into()
            .map_err(|_| ParseError::SyntaxError)?;
        Ok(Self {
            before,
            instruction,
            after,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ChronalClassification {
    test_cases: Vec<TestCase>,
    instructions: Vec<Instruction>,
}

impl FromStr for ChronalClassification {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.splitn(2, "\n\n\n\n");
        let test_cases = parts
            .next()
            .into_iter()
            .flat_map(|upper| upper.split("\n\n").map(str::parse))
            .collect::<Result<Vec<_>, _>>()?;
        let instructions = parts
            .next()
            .into_iter()
            .flat_map(|upper| upper.lines().map(str::parse))
            .collect::<Result<Vec<_>, _>>()?;
        if parts.next().is_some() {
            return Err(ParseError::SyntaxError);
        }
        Ok(Self {
            test_cases,
            instructions,
        })
    }
}

#[aoc_generator(day16)]
fn parse(input: &str) -> Result<ChronalClassification, ParseError> {
    input.parse()
}

#[aoc(day16, part1)]
fn part_1(input: &ChronalClassification) -> usize {
    let mut multiop_testcases = 0;
    for test_case in &input.test_cases {
        let &TestCase {
            before,
            instruction,
            after,
        } = test_case;
        let Instruction { a, b, c, .. } = instruction;
        let mut op_count = 16;
        for opcode in Opcode::all() {
            let mut registers = before;
            opcode.execute(a, b, c, &mut registers);
            if registers != after {
                op_count -= 1;
            }
        }
        if op_count >= 3 {
            multiop_testcases += 1;
        }
    }
    multiop_testcases
}

#[aoc(day16, part2)]
fn part_2(input: &ChronalClassification) -> u32 {
    let mut possible_opcodes_per_opnum = [u16::MAX; 16];
    for test_case in &input.test_cases {
        let &TestCase {
            before,
            instruction,
            after,
        } = test_case;
        let Instruction { op, a, b, c } = instruction;
        for opcode in Opcode::all() {
            let mut registers = before;
            opcode.execute(a, b, c, &mut registers);
            if registers != after {
                possible_opcodes_per_opnum[op as usize] &= !(1 << opcode as u8);
            }
        }
    }
    let Some(mapping) = find_mapping(&possible_opcodes_per_opnum, u16::MAX, 0) else {
        println!("No mapping found");
        return 0;
    };
    let mut machine = Machine::new(&input.instructions, mapping);
    machine.run()
}

fn find_mapping(
    possible_opcodes_per_opnum: &[u16; 16],
    remaining_opcodes: u16,
    index: usize,
) -> Option<[Opcode; 16]> {
    if index >= 16 {
        return Some([Opcode::AddI; 16]);
    }
    for opcode in Opcode::all() {
        let bit = 1 << opcode as u8;
        if possible_opcodes_per_opnum[index] & remaining_opcodes & bit != 0
            && let Some(mut res) = find_mapping(
                possible_opcodes_per_opnum,
                remaining_opcodes & !bit,
                index + 1,
            )
        {
            res[index] = opcode;
            return Some(res);
        }
    }
    None
}

struct Machine<'a> {
    instructions: &'a [Instruction],
    mapping: [Opcode; 16],
    registers: [u32; 4],
    ip: usize,
}

impl<'a> Machine<'a> {
    const fn new(instructions: &'a [Instruction], mapping: [Opcode; 16]) -> Self {
        Self {
            instructions,
            mapping,
            registers: [0; 4],
            ip: 0,
        }
    }

    fn step(&mut self) -> bool {
        let Instruction { op, a, b, c } = self.instructions[self.ip];
        self.mapping[op as usize].execute(a, b, c, &mut self.registers);
        self.ip += 1;
        self.ip < self.instructions.len()
    }

    fn run(&mut self) -> u32 {
        while self.step() {}
        self.registers[0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        Before: [3, 2, 1, 1]\n\
        9 2 1 2\n\
        After:  [3, 2, 2, 1]\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(
            result,
            ChronalClassification {
                test_cases: vec![TestCase {
                    before: [3, 2, 1, 1],
                    instruction: Instruction {
                        op: 9,
                        a: 2,
                        b: 1,
                        c: 2
                    },
                    after: [3, 2, 2, 1]
                }],
                instructions: vec![]
            }
        );
    }

    #[test]
    fn test_part_1() {
        let input = parse(EXAMPLE).unwrap();
        let result = part_1(&input);
        assert_eq!(result, 1);
    }
}
