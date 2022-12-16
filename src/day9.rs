use crate::prelude::{run, Coord, PuzzleInput};
use itertools::{repeat_n, Itertools, RepeatN};

type RopePath = Vec<Direction>;

/// Direction as parsed from the puzzle input.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Direction {
    Up,
    Down,
    Right,
    Left,
}

impl Direction {
    /// Each line contains multiple repetitions of the same instruction.
    fn from(line: String) -> Option<RepeatN<Direction>> {
        let split: Vec<&str> = line.split_whitespace().collect();
        match split[0] {
            "U" => Some(repeat_n(Direction::Up, split[1].parse().ok()?)),
            "D" => Some(repeat_n(Direction::Down, split[1].parse().ok()?)),
            "R" => Some(repeat_n(Direction::Right, split[1].parse().ok()?)),
            "L" => Some(repeat_n(Direction::Left, split[1].parse().ok()?)),
            _ => None,
        }
    }
}

/// Conflate position and translation vectors.
impl From<&Direction> for Coord {
    fn from(dir: &Direction) -> Coord {
        match dir {
            Direction::Up => Coord::new(0, 1),
            Direction::Down => Coord::new(0, -1),
            Direction::Left => Coord::new(-1, 0),
            Direction::Right => Coord::new(1, 0),
        }
    }
}

/// Add positition and translation representation.
impl std::ops::AddAssign<&Direction> for Coord {
    fn add_assign(&mut self, rhs: &Direction) {
        *self += Coord::from(rhs)
    }
}

pub fn read_data(lines: PuzzleInput) -> RopePath {
    lines
        .into_iter()
        .filter_map(Direction::from)
        .flatten()
        .collect()
}

/// Calculate next tail position based on current and head positions.
fn follow(head: Coord, tail: &Coord) -> Coord {
    if tail.chebyshev(head) > 1 {
        let diff = head - tail;
        return Coord::new(tail.x() + diff.x().signum(), tail.y() + diff.y().signum());
    }
    *tail
}

/// Application of two paramorphisms - one to move the head around according to its previous state
/// and the next direction, and a second one updating the tail position based on the head.
pub fn part1(steps: &RopePath) -> usize {
    steps
        .iter()
        .scan(Coord::ORIGIN, |pos, dir| {
            *pos += dir;
            Some(*pos)
        })
        .scan(Coord::ORIGIN, |pos, head| {
            *pos = follow(head, pos);
            Some(*pos)
        })
        .unique()
        .count()
}

/// For part 2, we essentially just chain the second paramorphism for each added segment, which
/// will simulate a trailing element to whatever path the predecessor traces.
pub fn part2(steps: &RopePath) -> usize {
    itertools::iterate(
        steps
            .iter()
            .scan(Coord::ORIGIN, |pos, dir| {
                *pos += dir;
                Some(*pos)
            })
            .collect::<Vec<_>>(), // Repeated collect/into_iter to avoid Scan<Scan<..>> type
        |segment_positions| {
            segment_positions
                .iter()
                .scan(Coord::ORIGIN, |pos, &pred| {
                    *pos = follow(pred, pos);
                    Some(*pos)
                })
                .collect::<Vec<_>>()
        },
    )
    .nth(9) // After nine tail segments, we're done.
    .expect("Could not calculate tail trajectory.")
    .into_iter()
    .unique()
    .count()
}

pub fn main() {
    let _ = run(
        9,
        read_data,
        part1,
        part2,
        [Some(13), Some(6337), Some(1), Some(2455)],
    );
}
