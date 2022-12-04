use crate::prelude::{run, PuzzleInput};

type Range = (u32, u32);
type Intermediate = Vec<(Range, Range)>;

pub fn read_data(lines: PuzzleInput) -> Intermediate {
    lines
        .into_iter()
        .map(|mline| {
            if let Ok(line) = mline {
                // Split and take first 2 ranges of each line.
                let elves = line
                    .split(",")
                    .take(2)
                    .map(|elf| {
                        // Within each line, split again and take first two as begin/end.
                        elf.split("-")
                            .take(2)
                            .map(|s| s.parse().expect("Not an integer!"))
                            .collect::<Vec<u32>>()
                    })
                    .collect::<Vec<_>>();
                // Transform to tuple for easier pattern matching.
                ((elves[0][0], elves[0][1]), (elves[1][0], elves[1][1]))
            } else {
                panic!("Invalid input!")
            }
        })
        .collect::<Vec<_>>()
}

/// Start of larger range is at most start of other, end of larger range is at least end of other.
pub fn part1(elves: &Intermediate) -> u32 {
    elves
        .iter()
        .filter(|((s1, e1), (s2, e2))| (s1 <= s2 && e1 >= e2) || (s2 <= s1 && e2 >= e1))
        .count() as u32
}

/// Entire first range is before start of second, or vice versa.
pub fn part2(elves: &Intermediate) -> u32 {
    elves
        .iter()
        .filter(|((s1, e1), (s2, e2))| !(e1 < s2 && s1 < s2) && !(e2 < s1 && s2 < s1))
        .count() as u32
}

pub fn main() {
    let _ = run(
        4,
        read_data,
        part1,
        part2,
        [Some(2), Some(459), Some(4), Some(779)],
    );
}
