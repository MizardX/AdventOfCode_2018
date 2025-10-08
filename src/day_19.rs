use std::fmt::Display;
use std::num::ParseIntError;
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Syntax error")]
    SyntaxError,
    #[error("Invalid operation")]
    InvalidOperation,
    #[error(transparent)]
    InvalidNumber(#[from] ParseIntError),
}

const NUM_REGISTERS: usize = 6;

#[derive(Debug, Clone, Copy, Eq)]
enum Reg {
    IP(u8),
    Num(u8),
}

impl Reg {
    const fn index(self) -> u8 {
        match self {
            Self::IP(n) | Self::Num(n) => n,
        }
    }

    const fn get_value(self, registers: &[u32; NUM_REGISTERS]) -> u32 {
        registers[self.index() as usize]
    }

    const fn set_value(self, value: u32, registers: &mut [u32; NUM_REGISTERS]) {
        registers[self.index() as usize] = value;
    }

    const fn replace_ip(&mut self, ip: u8) {
        if let Self::Num(n) = *self
            && n == ip
        {
            *self = Self::IP(n);
        }
    }
}

impl PartialEq for Reg {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}

impl FromStr for Reg {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::Num(s.parse()?))
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IP(..) => f.write_str("ip"),
            Self::Num(n) => write!(f, "r{n}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegOrValue {
    Reg(Reg),
    Value(u8),
}

impl RegOrValue {
    const fn get_value(self, registers: &[u32; NUM_REGISTERS]) -> u32 {
        match self {
            Self::Reg(reg) => reg.get_value(registers),
            Self::Value(val) => val as u32,
        }
    }

    const fn replace_ip(&mut self, ip: u8) {
        if let Self::Reg(reg) = self {
            reg.replace_ip(ip);
        }
    }
}

impl Display for RegOrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => reg.fmt(f),
            Self::Value(val) => write!(f, "{val}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOp {
    Add,
    Mul,
    BAn,
    BOr,
    Rem,
}

impl BinOp {
    const fn execute(self, a: u32, b: u32) -> u32 {
        match self {
            Self::Add => a + b,
            Self::Mul => a * b,
            Self::BAn => a & b,
            Self::BOr => a | b,
            Self::Rem => a % b,
        }
    }
}

impl FromStr for BinOp {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "addr" | "addi" => Self::Add,
            "mulr" | "muli" => Self::Mul,
            "banr" | "bani" => Self::BAn,
            "borr" | "bori" => Self::BOr,

            "remr" | "remi" => Self::Rem,
            _ => return Err(ParseError::InvalidOperation),
        })
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Add => "add",
            Self::Mul => "mul",
            Self::BAn => "ban",
            Self::BOr => "bor",
            Self::Rem => "rem",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmpOp {
    Eq,
    Gt,
}

impl CmpOp {
    const fn execute(self, a: u32, b: u32) -> u32 {
        (match self {
            Self::Eq => a == b,
            Self::Gt => a > b,
        }) as u32
    }
}

impl FromStr for CmpOp {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "gtir" | "gtri" | "gtrr" => Self::Gt,
            "eqir" | "eqri" | "eqrr" => Self::Eq,
            _ => return Err(ParseError::InvalidOperation),
        })
    }
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Eq => "eq",
            Self::Gt => "gt",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    BinOp(BinOp, Reg, RegOrValue, Reg),
    CmpOp(CmpOp, RegOrValue, RegOrValue, Reg),
    Set(RegOrValue, Reg),
}

impl Instruction {
    const fn execute(self, registers: &mut [u32; NUM_REGISTERS]) {
        match self {
            Self::BinOp(op, a, b, c) => {
                c.set_value(
                    op.execute(a.get_value(registers), b.get_value(registers)),
                    registers,
                );
            }
            Self::CmpOp(op, a, b, c) => {
                c.set_value(
                    op.execute(a.get_value(registers), b.get_value(registers)),
                    registers,
                );
            }
            Self::Set(a, c) => c.set_value(a.get_value(registers), registers),
        }
    }
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut words = s.split_ascii_whitespace();
        let op = words.next().ok_or(ParseError::SyntaxError)?;
        let a = words.next().ok_or(ParseError::SyntaxError)?;
        let b = words.next().ok_or(ParseError::SyntaxError)?;
        let c = words.next().ok_or(ParseError::SyntaxError)?;
        Ok(match op {
            "addr" | "mulr" | "banr" | "borr" | "remr" => Self::BinOp(
                op.parse()?,
                a.parse()?,
                RegOrValue::Reg(b.parse()?),
                c.parse()?,
            ),
            "addi" | "muli" | "bani" | "bori" | "remi" => Self::BinOp(
                op.parse()?,
                a.parse()?,
                RegOrValue::Value(b.parse()?),
                c.parse()?,
            ),
            "setr" => Self::Set(RegOrValue::Reg(a.parse()?), c.parse()?),
            "seti" => Self::Set(RegOrValue::Value(a.parse()?), c.parse()?),
            "gtir" | "eqir" => Self::CmpOp(
                op.parse()?,
                RegOrValue::Value(a.parse()?),
                RegOrValue::Reg(b.parse()?),
                c.parse()?,
            ),
            "gtri" | "eqri" => Self::CmpOp(
                op.parse()?,
                RegOrValue::Reg(a.parse()?),
                RegOrValue::Value(b.parse()?),
                c.parse()?,
            ),
            "gtrr" | "eqrr" => Self::CmpOp(
                op.parse()?,
                RegOrValue::Reg(a.parse()?),
                RegOrValue::Reg(b.parse()?),
                c.parse()?,
            ),
            _ => return Err(ParseError::InvalidOperation),
        })
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::BinOp(op, a, RegOrValue::Reg(b), c) => write!(f, "{op}r {a} {b} {c}"),
            Self::BinOp(op, a, RegOrValue::Value(b), c) => write!(f, "{op}i {a} {b} {c}"),
            Self::CmpOp(op, RegOrValue::Reg(a), RegOrValue::Reg(b), c) => {
                write!(f, "{op}rr {a} {b} {c}")
            }
            Self::CmpOp(op, RegOrValue::Reg(a), RegOrValue::Value(b), c) => {
                write!(f, "{op}ri {a} {b} {c}")
            }
            Self::CmpOp(op, RegOrValue::Value(a), RegOrValue::Reg(b), c) => {
                write!(f, "{op}ir {a} {b} {c}")
            }
            Self::Set(RegOrValue::Reg(a), c) => write!(f, "setr {a} _ {c}"),
            Self::Set(RegOrValue::Value(a), c) => write!(f, "seti {a} _ {c}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Program {
    instructions: Vec<Instruction>,
    ip_register: u8,
}

impl FromStr for Program {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();
        let ip_register = lines
            .next()
            .ok_or(ParseError::SyntaxError)?
            .strip_prefix("#ip ")
            .ok_or(ParseError::SyntaxError)?
            .parse()?;
        let mut instructions = lines.map(str::parse).collect::<Result<Vec<Instruction>, _>>()?;
        for instr in &mut instructions {
            match instr {
                Instruction::BinOp(_, a, b, c) => {
                    a.replace_ip(ip_register);
                    b.replace_ip(ip_register);
                    c.replace_ip(ip_register);
                }
                Instruction::CmpOp(_, a, b, c) => {
                    a.replace_ip(ip_register);
                    b.replace_ip(ip_register);
                    c.replace_ip(ip_register);
                }
                Instruction::Set(a, c) => {
                    a.replace_ip(ip_register);
                    c.replace_ip(ip_register);
                }
            }
        }
        Ok(Self {
            instructions,
            ip_register,
        })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#ip {}", self.ip_register)?;
        for (i, instr) in self.instructions.iter().enumerate() {
            writeln!(f)?;
            write!(f, "[{i:3}] {instr}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Machine<'a> {
    program: &'a Program,
    registers: [u32; NUM_REGISTERS],
    steps: usize,
    logging: bool,
}

impl<'a> Machine<'a> {
    const fn new(program: &'a Program, logging: bool) -> Self {
        Self {
            program,
            registers: [0; NUM_REGISTERS],
            steps: 0,
            logging,
        }
    }

    fn step(&mut self) {
        let ip_reg = self.program.ip_register as usize;
        let ip = self.registers[ip_reg] as usize;
        let instr = &self.program.instructions[ip];
        if self.logging {
            println!("[{ip:3}] {:3?} {instr}", &self.registers);
        }
        instr.execute(&mut self.registers);
        self.registers[ip_reg] += 1;
        self.steps += 1;
    }

    fn run(&mut self) -> u32 {
        let ip_reg = self.program.ip_register as usize;
        let end = self.program.instructions.len();
        while (self.registers[ip_reg] as usize) < end {
            self.step();
        }
        self.registers[0]
    }
}

#[aoc_generator(day19)]
fn parse(input: &str) -> Result<Program, ParseError> {
    input.parse()
}

#[aoc(day19, part1)]
fn part_1(program: &Program) -> u32 {
    let program = optimize(program, false).unwrap_or_else(|| program.clone());
    let mut machine = Machine::new(&program, false);
    machine.run()
}

#[aoc(day19, part2)]
fn part_2(program: &Program) -> u32 {
    let optimized = optimize(program, false).expect("Unable to optimize program");
    let mut machine = Machine::new(&optimized, false);
    machine.registers[0] = 1;
    machine.run()
}

const TARGET_IP: Reg = Reg::IP(3);
const OPTIMIZE_TARGET: [Instruction; 10] = [
    // for r2 = 1; r2 <= 4; r2++) {
    Instruction::Set(RegOrValue::Value(1), Reg::Num(2)),
    // if r5 * r2 == r4 {
    Instruction::BinOp(
        BinOp::Mul,
        Reg::Num(5),
        RegOrValue::Reg(Reg::Num(2)),
        Reg::Num(1),
    ),
    Instruction::CmpOp(
        CmpOp::Eq,
        RegOrValue::Reg(Reg::Num(1)),
        RegOrValue::Reg(Reg::Num(4)),
        Reg::Num(1),
    ),
    Instruction::BinOp(
        BinOp::Add,
        Reg::Num(1),
        RegOrValue::Reg(Reg::Num(3)),
        TARGET_IP,
    ),
    Instruction::BinOp(BinOp::Add, Reg::Num(3), RegOrValue::Value(1), TARGET_IP),
    // r0 += r5
    Instruction::BinOp(
        BinOp::Add,
        Reg::Num(5),
        RegOrValue::Reg(Reg::Num(0)),
        Reg::Num(0),
    ),
    // }
    Instruction::BinOp(BinOp::Add, Reg::Num(2), RegOrValue::Value(1), Reg::Num(2)),
    Instruction::CmpOp(
        CmpOp::Gt,
        RegOrValue::Reg(Reg::Num(2)),
        RegOrValue::Reg(Reg::Num(4)),
        Reg::Num(1),
    ),
    Instruction::BinOp(
        BinOp::Add,
        TARGET_IP,
        RegOrValue::Reg(Reg::Num(1)),
        TARGET_IP,
    ),
    Instruction::Set(RegOrValue::Value(2), TARGET_IP),
    // }
];
const OPTIMIZE_REPLACEMENT: [Instruction; 4] = [
    // if (r5 % r5 == 0) {
    Instruction::BinOp(
        BinOp::Rem,
        Reg::Num(4),
        RegOrValue::Reg(Reg::Num(5)),
        Reg::Num(1),
    ),
    Instruction::CmpOp(
        CmpOp::Gt,
        RegOrValue::Reg(Reg::Num(1)),
        RegOrValue::Value(0),
        Reg::Num(1),
    ),
    Instruction::BinOp(
        BinOp::Add,
        TARGET_IP,
        RegOrValue::Reg(Reg::Num(1)),
        TARGET_IP,
    ),
    // r0 += r5
    Instruction::BinOp(
        BinOp::Add,
        Reg::Num(5),
        RegOrValue::Reg(Reg::Num(0)),
        Reg::Num(0),
    ),
    // }
];

fn optimize(program: &Program, logging: bool) -> Option<Program> {
    let padding = Instruction::BinOp(BinOp::Add, TARGET_IP, RegOrValue::Value(0), TARGET_IP);
    for offset in 0..program
        .instructions
        .len()
        .saturating_sub(OPTIMIZE_TARGET.len())
    {
        if let Some(mapping) = find_register_mapping(
            &program.instructions[offset..offset + OPTIMIZE_TARGET.len()],
            Reg::IP(program.ip_register),
            &OPTIMIZE_TARGET,
            TARGET_IP,
        ) {
            let mut instructions = Vec::with_capacity(program.instructions.len());
            instructions.extend_from_slice(&program.instructions[..offset]);
            for &instr in OPTIMIZE_REPLACEMENT.iter().chain(std::iter::repeat_n(
                &padding,
                OPTIMIZE_TARGET.len() - OPTIMIZE_REPLACEMENT.len(),
            )) {
                instructions.push(match instr {
                    Instruction::BinOp(op, a, b, c) => Instruction::BinOp(
                        op,
                        mapping.reverse_reg(a).unwrap(),
                        mapping.reverse_reg_or_value(b).unwrap(),
                        mapping.reverse_reg(c).unwrap(),
                    ),
                    Instruction::CmpOp(op, a, b, c) => Instruction::CmpOp(
                        op,
                        mapping.reverse_reg_or_value(a).unwrap(),
                        mapping.reverse_reg_or_value(b).unwrap(),
                        mapping.reverse_reg(c).unwrap(),
                    ),
                    Instruction::Set(a, c) => Instruction::Set(
                        mapping.reverse_reg_or_value(a).unwrap(),
                        mapping.reverse_reg(c).unwrap(),
                    ),
                });
            }
            instructions.extend_from_slice(&program.instructions[offset + OPTIMIZE_TARGET.len()..]);
            let ip_register = mapping
                .reverse_reg(Reg::IP(program.ip_register))
                .unwrap()
                .index();
            let new_program = Program {
                instructions,
                ip_register,
            };
            if logging {
                println!("BEFORE:");
                println!("{program}");
                println!("AFTER:");
                println!("{new_program}");
            }
            return Some(new_program);
        }
    }
    None
}

fn find_register_mapping(
    source: &[Instruction],
    soruce_ip: Reg,
    target: &[Instruction],
    target_ip: Reg,
) -> Option<RegisterMapping> {
    let mut mapping = RegisterMapping::new();
    mapping.try_insert(soruce_ip, target_ip);
    for (&instr1, &instr2) in source.iter().zip(target) {
        // println!("Trying to match `{instr1}` to `{instr2}`");
        match (instr1, instr2) {
            (Instruction::BinOp(op1, a1, b1, c1), Instruction::BinOp(op2, a2, b2, c2)) if op1 == op2 => {
                if !mapping.try_insert(a1, a2) {
                    // println!(" -> a: unable to map {a1} to {a2}");
                    return None;
                }
                if !mapping.try_insert_rv(b1, b2) {
                    // println!(" -> b: unable to map {b1} to {b2}");
                    return None;
                }
                if !mapping.try_insert(c1, c2) {
                    // println!(" -> c: unable to map {c1} to {c2}");
                    return None;
                }
            }
            (Instruction::CmpOp(op1, a1, b1, c1), Instruction::CmpOp(op2, a2, b2, c2)) if op1 == op2 => {
                if !mapping.try_insert_rv(a1, a2) {
                    // println!(" -> a: unable to map {a1} to {a2}");
                    return None;
                }
                if !mapping.try_insert_rv(b1, b2) {
                    // println!(" -> b: unable to map {b1} to {b2}");
                    return None;
                }
                if !mapping.try_insert(c1, c2) {
                    // println!(" -> c: unable to map {c1} to {c2}");
                    return None;
                }
            }
            (Instruction::Set(a1, c1), Instruction::Set(a2, c2)) => {
                if !mapping.try_insert_rv(a1, a2) {
                    // println!(" -> a: unable to map {a1} to {a2}");
                    return None;
                }
                if !mapping.try_insert(c1, c2) {
                    // println!(" -> c: unable to map {c1} to {c2}");
                    return None;
                }
            }
            _ => {
                // println!(" -> unable to match optypes");
                return None;
            }
        }
    }
    Some(mapping)
}

#[derive(Debug, Clone, Default)]
struct RegisterMapping {
    forward: [Option<Reg>; NUM_REGISTERS],
    reverse: [Option<Reg>; NUM_REGISTERS],
}

impl RegisterMapping {
    fn new() -> Self {
        Self::default()
    }
    fn try_insert(&mut self, reg1: Reg, reg2: Reg) -> bool {
        match (
            self.forward[reg1.index() as usize],
            self.reverse[reg2.index() as usize],
        ) {
            (Some(f), Some(r)) if f == reg2 && r == reg1 => true,
            (None, None) => {
                self.forward[reg1.index() as usize] = Some(reg2);
                self.reverse[reg2.index() as usize] = Some(reg1);
                true
            }
            _ => false,
        }
    }
    fn try_insert_rv(&mut self, lhs: RegOrValue, rhs: RegOrValue) -> bool {
        match (lhs, rhs) {
            (RegOrValue::Reg(reg1), RegOrValue::Reg(reg2)) => self.try_insert(reg1, reg2),
            (RegOrValue::Value(v1), RegOrValue::Value(v2)) => v1 == v2,
            _ => false,
        }
    }
    const fn reverse_reg(&self, reg: Reg) -> Option<Reg> {
        self.reverse[reg.index() as usize]
    }
    fn reverse_reg_or_value(&self, mut val: RegOrValue) -> Option<RegOrValue> {
        match val {
            RegOrValue::Value(_) => {}
            RegOrValue::Reg(ref mut reg) => {
                *reg = self.reverse_reg(*reg)?;
            }
        }
        Some(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        #ip 0\n\
        seti 5 0 1\n\
        seti 6 0 2\n\
        addi 0 1 0\n\
        addr 1 2 3\n\
        setr 1 0 0\n\
        seti 8 0 4\n\
        seti 9 0 5\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        assert_eq!(result.ip_register, 0);
        assert_eq!(
            result.instructions,
            [
                Instruction::Set(RegOrValue::Value(5), Reg::Num(1)),
                Instruction::Set(RegOrValue::Value(6), Reg::Num(2)),
                Instruction::BinOp(BinOp::Add, Reg::IP(0), RegOrValue::Value(1), Reg::IP(0)),
                Instruction::BinOp(
                    BinOp::Add,
                    Reg::Num(1),
                    RegOrValue::Reg(Reg::Num(2)),
                    Reg::Num(3)
                ),
                Instruction::Set(RegOrValue::Reg(Reg::Num(1)), Reg::IP(0)),
                Instruction::Set(RegOrValue::Value(8), Reg::Num(4)),
                Instruction::Set(RegOrValue::Value(9), Reg::Num(5)),
            ]
        );
    }

    #[test]
    fn test_part_1() {
        let program = parse(EXAMPLE).unwrap();
        let result = part_1(&program);
        assert_eq!(result, 7);
    }
}
