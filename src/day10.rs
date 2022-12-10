use crate::prelude::{print_display, read_display, run, PuzzleInput};
use itertools::Itertools;

pub enum Instruction {
    NoOp,
    AddX(i32),
}

impl Instruction {
    fn read(line: String) -> Option<Instruction> {
        if line == "noop" {
            Some(Instruction::NoOp)
        } else {
            Some(Instruction::AddX(line[5..].parse().ok()?))
        }
    }
}

pub type Program = Vec<Instruction>;

pub fn read_data(lines: PuzzleInput) -> Program {
    lines
        .into_iter()
        .map(Instruction::read)
        .collect::<Option<Vec<Instruction>>>()
        .expect("Failed to read puzzle input")
}

pub fn part1(instructions: &Program) -> String {
    format!(
        "{}",
        instructions
            .iter()
            .scan(1, |x, instr| match instr {
                Instruction::NoOp => Some(vec![*x]),
                Instruction::AddX(delta_x) => {
                    let r = Some(vec![*x, *x]);
                    *x += delta_x;
                    r
                }
            })
            .collect::<Vec<Vec<i32>>>()
            .concat()
            .into_iter()
            .enumerate()
            .skip(19)
            .step_by(40)
            .take(6)
            .map(|(a, b)| (a + 1) as i32 * b)
            .sum::<i32>()
    )
}

pub fn part2(instructions: &Program) -> String {
    let display: Vec<Vec<bool>> = instructions
        .iter()
        .scan(1, |x, instr| match instr {
            Instruction::NoOp => Some(vec![*x]),
            Instruction::AddX(delta_x) => {
                let r = Some(vec![*x, *x]);
                *x += delta_x;
                r
            }
        })
        .collect::<Vec<Vec<i32>>>()
        .concat()
        .into_iter()
        .enumerate()
        .map(|(sprite, x)| (x - (sprite % 40) as i32).abs() <= 1)
        .chunks(40)
        .into_iter()
        .map(|l| l.collect())
        .collect();

    #[cfg(not(feature = "timing"))]
    print_display(display.clone());

    read_display(display)
}

pub fn main() {
    let _ = run(
        10,
        read_data,
        part1,
        part2,
        [
            Some("13140".to_string()),
            Some("14920".to_string()),
            Some("????????".to_string()),
            Some("BUCACBUZ".to_string()),
        ],
    );
}
