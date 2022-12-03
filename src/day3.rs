use crate::prelude::{run, PuzzleInput};
use array_tool::vec::{Intersect, Union};

type Intermediate = Vec<(Vec<char>, Vec<char>)>;

pub fn read_data(lines: PuzzleInput) -> Intermediate {
    lines
        .into_iter()
        .map(|l| {
            if let Ok(line) = l {
                let split = line.len() / 2;
                let (compartment1, compartment2) = line.split_at(split);
                (
                    compartment1.chars().collect::<Vec<_>>(),
                    compartment2.chars().collect::<Vec<_>>(),
                )
            } else {
                panic!()
            }
        })
        .collect::<Vec<_>>()
}

fn priority(c: &char) -> u32 {
    match c {
        'a'..='z' => *c as u32 - 'a' as u32 + 1,
        'A'..='Z' => *c as u32 - 'A' as u32 + 27,
        _ => 0,
    }
}

pub fn part1(doubles: &Intermediate) -> u32 {
    doubles
        .into_iter()
        .map(|(compartment1, compartment2)| compartment1.intersect(compartment2.to_vec()))
        .collect::<Vec<_>>()
        .concat()
        .iter()
        .map(priority)
        .sum()
}

pub fn part2(doubles: &Intermediate) -> u32 {
    doubles
        .into_iter()
        .map(|(compartment1, compartment2)| compartment1.union(compartment2.to_vec()))
        .collect::<Vec<_>>()
        .chunks(3)
        .into_iter()
        .map(|group| {
            group[0]
                .intersect(group[1].clone())
                .intersect(group[2].clone())
        })
        .collect::<Vec<_>>()
        .concat()
        .iter()
        .map(priority)
        .sum()
}

pub fn main() {
    let _ = run(
        3,
        read_data,
        part1,
        part2,
        [Some(157), Some(8252), Some(70), Some(2828)],
    );
}
