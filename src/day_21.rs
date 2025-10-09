use std::collections::HashSet;
use std::fmt::Display;
use std::num::ParseIntError;
use std::ops::{Index, IndexMut};
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
    Value(u32),
}

impl RegOrValue {
    const fn get_value(self, registers: &[u32; NUM_REGISTERS]) -> u32 {
        match self {
            Self::Reg(reg) => reg.get_value(registers),
            Self::Value(val) => val,
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
        let mut instructions = lines
            .map(str::parse)
            .collect::<Result<Vec<Instruction>, _>>()?;
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

    const fn ip(&self) -> usize {
        self.registers[self.program.ip_register as usize] as usize
    }

    fn step(&mut self) {
        let ip_reg = self.program.ip_register as usize;
        let ip = self.registers[ip_reg] as usize;
        let instr = &self.program.instructions[ip];
        if self.logging {
            println!("{self}");
        }
        instr.execute(&mut self.registers);
        self.registers[ip_reg] += 1;
        self.steps += 1;
    }

    #[allow(unused)]
    fn run(&mut self) -> u32 {
        let end = self.program.instructions.len();
        while self.ip() < end {
            self.step();
        }
        self.registers[0]
    }
}

impl Index<Reg> for Machine<'_> {
    type Output = u32;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.registers[index.index() as usize]
    }
}

impl IndexMut<Reg> for Machine<'_> {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self.registers[index.index() as usize]
    }
}

impl Display for Machine<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ip = self.ip();
        let instr = &self.program.instructions[ip];
        write!(f, "[{ip:3}] {:3?} {instr}", &self.registers)
    }
}

#[aoc_generator(day21)]
fn parse(input: &str) -> Result<Program, ParseError> {
    input.parse()
}

#[aoc(day21, part1)]
fn part_1(program: &Program) -> u32 {
    let mut machine = Machine::new(program, false);
    let pos = machine.program.instructions.len() - 3;

    if let Instruction::CmpOp(CmpOp::Eq, RegOrValue::Reg(reg1), RegOrValue::Reg(reg2), _) =
        machine.program.instructions[pos]
    {
        let target_reg = if reg1 == Reg::Num(0) { reg2 } else { reg1 };
        while machine.ip() != pos {
            machine.step();
        }
        machine[target_reg]
    } else {
        0
    }
}

#[aoc(day21, part2)]
fn part_2(program: &Program) -> u32 {
    let mut machine = Machine::new(program, false);
    let pos = machine.program.instructions.len() - 3;

    let mut seen = HashSet::new();


    if let Instruction::CmpOp(CmpOp::Eq, RegOrValue::Reg(reg1), RegOrValue::Reg(reg2), _) =
        machine.program.instructions[pos]
    {
        let target_reg = if reg1 == Reg::Num(0) { reg2 } else { reg1 };
        let mut prev_value = 0;
        for _ in 0..100_000 {
            while machine.ip() != pos {
                machine.step();
            }
            let val = machine[target_reg];
            if !seen.insert(val) {
                return prev_value;
            }
            prev_value = val;
            machine.step();
        }
    }
    0
}
