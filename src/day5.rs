use crate::pest::Parser;
use crate::prelude::{run, PuzzleInput};
use std::ops::IndexMut;

type Intermediate = (Vec<Step>, Stacks);
type Res = String;

#[derive(Parser)]
#[grammar = "day5.pest"]
struct MyParser;

#[derive(Debug)]
pub struct Step {
    num: usize,
    from: usize,
    to: usize,
}

impl Step {
    fn read_steps<I: Iterator<Item = String>>(iterator: &mut I) -> Vec<Step> {
        let mut steps = iterator
            .by_ref()
            .take_while(|s| s != "")
            .map(|line| Step::read(line))
            .collect::<Vec<_>>();
        steps.reverse();
        steps
    }

    fn read(line: String) -> Step {
        let mut res = MyParser::parse(Rule::step, &line)
            .expect("No parse for step!")
            .next()
            .expect("Empty result!")
            .into_inner();
        Step {
            num: res.next().unwrap().as_str().parse().unwrap(),
            from: res.next().unwrap().as_str().parse().unwrap(),
            to: res.next().unwrap().as_str().parse().unwrap(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Stacks {
    stacks: Vec<Vec<char>>,
}

impl Stacks {
    fn read<I: Iterator<Item = String>>(iterator: &mut I) -> Stacks {
        let num_stacks = iterator
            .next()
            .unwrap()
            .chars()
            .filter(|c| c.is_numeric())
            .count();

        let mut stacks: Vec<Vec<char>> = vec![Vec::new(); num_stacks];

        iterator.map(|line| Stacks::read_row(line)).for_each(|row| {
            row.iter().enumerate().for_each(|(idx, mcrate)| {
                if let Some(some_crate) = mcrate {
                    stacks[idx].push(*some_crate);
                }
            });
        });
        Stacks { stacks }
    }

    fn read_row(line: String) -> Vec<Option<char>> {
        let rows = line.chars().collect::<Vec<char>>();
        rows.chunks(4)
            .map(|chunk| {
                MyParser::parse(Rule::maybe_crate, &String::from_iter(chunk))
                    .expect("No parse for step!")
                    .next()
                    .expect("Empty result!")
                    .into_inner()
                    .next()
                    .map(|m| m.as_str().to_owned().chars().nth(1).unwrap())
            })
            .collect()
    }

    fn apply_step(&mut self, step: &Step) {
        (0..step.num).for_each(|_| {
            let src: &mut Vec<char> = &mut self.stacks.index_mut(step.from - 1);
            let val = src.pop().unwrap();
            let tar: &mut Vec<char> = &mut self.stacks.index_mut(step.to - 1);
            tar.push(val);
        })
    }

    fn apply_step_9001(&mut self, step: &Step) {
        let src: &mut Vec<char> = &mut self.stacks.index_mut(step.from - 1);
        let vals = src.drain(src.len() - step.num..).collect::<Vec<char>>();
        let tar: &mut Vec<char> = &mut self.stacks.index_mut(step.to - 1);
        tar.extend(vals);
    }
}

pub fn read_data(lines: PuzzleInput) -> Intermediate {
    let owned_data = lines.map(|l| l.unwrap()).collect::<Vec<String>>();
    let mut iterator = owned_data.into_iter().rev();

    (Step::read_steps(&mut iterator), Stacks::read(&mut iterator))
}

pub fn part1((steps, stacks): &Intermediate) -> Res {
    let mut stacks = stacks.clone();
    steps
        .iter()
        .for_each(|step| stacks.apply_step(step.clone()));
    String::from_iter(stacks.stacks.iter().map(|s| s.iter().last().unwrap()))
}

pub fn part2((steps, stacks): &Intermediate) -> Res {
    let mut stacks = stacks.clone();
    steps
        .iter()
        .for_each(|step| stacks.apply_step_9001(step.clone()));
    String::from_iter(stacks.stacks.iter().map(|s| s.iter().last().unwrap()))
}

pub fn main() {
    let _ = run(
        5,
        read_data,
        part1,
        part2,
        [
            Some("CMZ".to_string()),
            Some("CWMTGHBDW".to_string()),
            Some("MCD".to_string()),
            Some("SSCGWJCRB".to_string()),
        ],
    );
}
