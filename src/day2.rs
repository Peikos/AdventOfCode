use crate::prelude::{run, PuzzleInput};

type Round = Vec<(Move, Move)>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Move {
    Rock,
    Paper,
    Scissors,
    X,
    Y,
    Z,
}

impl Move {
    pub fn read(input: &str) -> Move {
        match input {
            "A" => Move::Rock,
            "B" => Move::Paper,
            "C" => Move::Scissors,
            " X" => Move::X,
            " Y" => Move::Y,
            " Z" => Move::Z,
            inp => panic!("Invalid input: {}", inp),
        }
    }

    /// Scoring can be expressed as a 3×3 matrix
    pub fn score(&self, them: &Move) -> u32 {
        #[allow(clippy::identity_op)]
        match (them, self) {
            (Move::Paper, Move::X) => 1 + 0,
            (Move::Scissors, Move::Y) => 2 + 0,
            (Move::Rock, Move::Z) => 3 + 0,
            (Move::Rock, Move::X) => 1 + 3,
            (Move::Paper, Move::Y) => 2 + 3,
            (Move::Scissors, Move::Z) => 3 + 3,
            (Move::Scissors, Move::X) => 1 + 6,
            (Move::Rock, Move::Y) => 2 + 6,
            (Move::Paper, Move::Z) => 3 + 6,
            _ => panic!(),
        }
    }

    /// The new improved rules in effect just shift each row of the scoring matrix left, right or 0.
    pub fn score2(&self, them: &Move) -> u32 {
        match them {
            Move::Rock => self.rotate().rotate().score(them),
            Move::Paper => self.score(them),
            Move::Scissors => self.rotate().score(them),
            _ => panic!(),
        }
    }
    /// Shifting a row is a rotation such that ρ² = ρ⁻¹, so only ρ suffices.
    fn rotate(&self) -> Self {
        match self {
            Move::X => Move::Y,
            Move::Y => Move::Z,
            Move::Z => Move::X,
            _ => panic!(),
        }
    }
}

pub fn read_data(lines: PuzzleInput) -> Round {
    lines
        .iter()
        .map(|l| {
            let (them, us) = l.split_at(1);
            (Move::read(them), Move::read(us))
        })
        .collect()
}

pub fn part1(rounds: &Round) -> u32 {
    rounds.iter().map(|(them, us)| us.score(them)).sum()
}

pub fn part2(rounds: &Round) -> u32 {
    rounds.iter().map(|(them, us)| us.score2(them)).sum()
}

pub fn main() {
    let _ = run(
        2,
        read_data,
        part1,
        part2,
        [Some(15), Some(11906), Some(12), Some(11186)],
    );
}
