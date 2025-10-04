use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::str::FromStr;

use thiserror::Error;

#[derive(Debug, Error)]
enum ParseError {
    #[error("Syntax error")]
    SyntaxError,
    #[error("Not a step name")]
    InvalidStep,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Requirement {
    prerequisite: usize,
    step: usize,
}

impl FromStr for Requirement {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rest = s.strip_prefix("Step ").ok_or(ParseError::SyntaxError)?;
        let (prerequisite, rest) = rest
            .split_once(" must be finished before step ")
            .ok_or(ParseError::SyntaxError)?;
        let step = rest
            .strip_suffix(" can begin.")
            .ok_or(ParseError::SyntaxError)?;
        if let &[step] = step.as_bytes()
            && step.is_ascii_uppercase()
            && let &[prerequisite] = prerequisite.as_bytes()
            && prerequisite.is_ascii_uppercase()
        {
            Ok(Self {
                prerequisite: (prerequisite - b'A') as usize,
                step: (step - b'A') as usize,
            })
        } else {
            Err(ParseError::InvalidStep)
        }
    }
}

#[aoc_generator(day7)]
fn parse(input: &str) -> Result<Vec<Requirement>, ParseError> {
    input.lines().map(str::parse).collect()
}

#[aoc(day7, part1)]
fn part_1(requirements: &[Requirement]) -> String {
    let sorted = topological_sort(requirements);
    unsafe {
        String::from_utf8_unchecked(
            sorted
                .into_iter()
                .map(|ix| u8::try_from(ix).unwrap() + b'A')
                .collect::<Vec<_>>(),
        )
    }
}

fn topological_sort(requirements: &[Requirement]) -> Vec<usize> {
    let mut nodes = condense_requirements(requirements);
    let mut pending: BinaryHeap<_> = nodes
        .iter()
        .enumerate()
        .filter_map(|(ix, node)| (node.seen && node.num_prerequisites == 0).then_some(Reverse(ix)))
        .collect();
    let mut sorted = Vec::new();
    while let Some(Reverse(next)) = pending.pop() {
        sorted.push(next);
        nodes[next].required_for.sort_unstable();
        for &child in &nodes[next].required_for {
            nodes[child].num_prerequisites -= 1;
            if nodes[child].num_prerequisites == 0 {
                pending.push(Reverse(child));
            }
        }
    }
    sorted
}

#[derive(Debug, Default)]
struct Node {
    num_prerequisites: usize,
    required_for: Vec<usize>,
    seen: bool,
}

fn condense_requirements(requirements: &[Requirement]) -> [Node; 26] {
    let mut nodes = [(); 26].map(|()| Node::default());
    for req in requirements {
        nodes[req.prerequisite].required_for.push(req.step);
        nodes[req.prerequisite].seen = true;
        nodes[req.step].num_prerequisites += 1;
        nodes[req.step].seen = true;
    }
    nodes
}

#[aoc(day7, part2)]
fn part_2(requirements: &[Requirement]) -> usize {
    simulate_construction(requirements, 5, 60)
}

fn simulate_construction(requirements: &[Requirement], num_workers: usize, delay_time: usize) -> usize {
    let mut nodes = condense_requirements(requirements);
    let mut free_workers = (0..num_workers).map(Reverse).collect::<BinaryHeap<_>>();
    let mut available_jobs: BinaryHeap<_> = nodes
        .iter()
        .enumerate()
        .filter_map(|(ix, node)| (node.seen && node.num_prerequisites == 0).then_some(Reverse(ix)))
        .collect();
    let mut job_completions = BinaryHeap::new();
    while let Some(&Reverse(worker)) = free_workers.peek()
        && let Some(&Reverse(job)) = available_jobs.peek()
    {
        free_workers.pop();
        available_jobs.pop();
        let time_complete = delay_time + job + 1; // 60s + job nr
        job_completions.push(Reverse((time_complete, job, worker)));
    }
    let mut time_spent = 0;
    while let Some(Reverse((time, job, worker))) = job_completions.pop() {
        time_spent = time;
        for &child in &nodes[job].required_for {
            nodes[child].num_prerequisites -= 1;
            if nodes[child].num_prerequisites == 0 {
                available_jobs.push(Reverse(child));
            }
        }
        free_workers.push(Reverse(worker));
        while let Some(&Reverse(worker)) = free_workers.peek()
            && let Some(&Reverse(job)) = available_jobs.peek()
        {
            free_workers.pop();
            available_jobs.pop();
            let time_complete = time + delay_time + job + 1;
            job_completions.push(Reverse((time_complete, job, worker)));
        }
    }
    time_spent
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
        Step C must be finished before step A can begin.\n\
        Step C must be finished before step F can begin.\n\
        Step A must be finished before step B can begin.\n\
        Step A must be finished before step D can begin.\n\
        Step B must be finished before step E can begin.\n\
        Step D must be finished before step E can begin.\n\
        Step F must be finished before step E can begin.\
    ";

    #[test]
    fn test_parse() {
        let result = parse(EXAMPLE).unwrap();
        let expected = [
            (b'C', b'A'),
            (b'C', b'F'),
            (b'A', b'B'),
            (b'A', b'D'),
            (b'B', b'E'),
            (b'D', b'E'),
            (b'F', b'E'),
        ]
        .map(|(prerequisite, step)| Requirement {
            prerequisite: usize::from(prerequisite - b'A'),
            step: usize::from(step - b'A'),
        });

        assert_eq!(result, expected);
    }

    #[test]
    fn test_part_1() {
        let requirements = parse(EXAMPLE).unwrap();
        let result = part_1(&requirements);
        assert_eq!(result, "CABDFE");
    }

    #[test]
    fn test_part_2() {
        let requirements = parse(EXAMPLE).unwrap();
        let result = simulate_construction(&requirements, 2, 0);
        assert_eq!(result, 15);
    }
}
