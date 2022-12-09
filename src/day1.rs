use crate::prelude::{run, PuzzleInput};
use itertools::Itertools;

type Calories = Vec<u32>;

pub fn read_data(lines: PuzzleInput) -> Calories {
    lines
        .into_iter()
        // Histomorphism over input vector, build new vec of totals.
        .fold(vec![0], |mut totals, line| {
            if line == *"" {
                // On an empty line, append a new count initialised at 0.
                totals.push(0);
            } else {
                // Otherwise, increment last count.
                let last_count: &mut u32 = totals.last_mut().expect("No count to increment");
                let calories = line.parse::<u32>().expect("No parse for calories count");
                *last_count += calories;
            }
            totals
        })
}

pub fn part1(calories: &Calories) -> u32 {
    *calories.iter().max().expect("No max value exists")
}

pub fn part2(calories: &Calories) -> u32 {
    calories.iter().sorted().rev().take(3).sum()
}

pub fn main() {
    let _ = run(
        1,
        read_data,
        part1,
        part2,
        [Some(24000), Some(69626), Some(45000), Some(206780)],
    );
}
