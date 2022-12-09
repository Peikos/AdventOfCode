use crate::prelude::{run, PuzzleInput};
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

/// Position in an x/y plane as a monoid, maybe move to prelude if this comes back more often.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Position {
    x: i32,
    y: i32,
}

impl std::ops::Add<Position> for &Position {
    type Output = Position;
    fn add(self, rhs: Position) -> Position {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Position {
    /// Zero element for positions at the origin.
    const ORIGIN: Position = Position { x: 0, y: 0 };

    /// Conflate position and translation vectors.
    fn from_direction(dir: Direction) -> Position {
        match dir {
            Direction::Up => Position { x: 0, y: 1 },
            Direction::Down => Position { x: 0, y: -1 },
            Direction::Left => Position { x: -1, y: 0 },
            Direction::Right => Position { x: 1, y: 0 },
        }
    }

    /// Add positition and translation representation.
    fn move_to(&self, dir: Direction) -> Position {
        self + Position::from_direction(dir)
    }

    /// L_∞ metric (as the king moves in chess).
    fn chebyshev_distance(&self, other: &Position) -> i32 {
        (self.x - other.x).abs().max((self.y - other.y).abs())
    }

    /// Calculate next tail position based on current and head positions.
    fn follow(&self, head: Position) -> Position {
        if self.chebyshev_distance(&head) > 1 {
            return Position {
                x: self.x + (head.x - self.x).signum(),
                y: self.y + (head.y - self.y).signum(),
            };
        }
        *self
    }
}

pub fn read_data(lines: PuzzleInput) -> RopePath {
    lines
        .into_iter()
        .filter_map(Direction::from)
        .flatten()
        .collect()
}

/// Application of two paramorphisms - one to move the head around according to its previous state
/// and the next direction, and a second one updating the tail position based on the head.
pub fn part1(steps: &RopePath) -> usize {
    steps
        .iter()
        .scan(Position::ORIGIN, |pos, dir| {
            *pos = pos.move_to(*dir);
            Some(*pos)
        })
        .scan(Position::ORIGIN, |pos, head| {
            *pos = pos.follow(head);
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
            .scan(Position::ORIGIN, |pos, dir| {
                *pos = pos.move_to(*dir);
                Some(*pos)
            })
            .collect::<Vec<_>>(), // Repeated collect/into_iter to avoid Scan<Scan<..>> type
        |segment_positions| {
            segment_positions
                .iter()
                .scan(Position::ORIGIN, |pos, &pred| {
                    *pos = pos.follow(pred);
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
