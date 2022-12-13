use crate::prelude::{run, ExtraIterators, PuzzleInput};
use itertools::{EitherOrBoth::*, Itertools};
use std::cmp::{Ordering, Ordering::*};
use std::iter::Peekable;

pub type Signal = Vec<Packet>;
pub type ParseInput<'a> = Peekable<std::str::Chars<'a>>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Packet {
    List(Vec<Packet>),
    Literal(u32),
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, rhs: &Packet) -> Option<Ordering> {
        match (self, rhs) {
            (Packet::List(xs), Packet::List(ys)) => xs
                .iter()
                .zip_longest(ys.iter())
                .map(|pair| match pair {
                    Both(x, y) => x.partial_cmp(y),
                    Left(_) => Some(Greater),
                    Right(_) => Some(Less),
                })
                .find(|c| *c != Some(Equal))
                .unwrap_or(Some(Equal)),
            (Packet::Literal(x), Packet::Literal(y)) => x.partial_cmp(y),
            (Packet::Literal(x), Packet::List(ys)) => {
                Packet::singleton(*x).partial_cmp(&Packet::List(ys.clone()))
            }
            (Packet::List(xs), Packet::Literal(y)) => {
                Packet::List(xs.clone()).partial_cmp(&Packet::singleton(*y))
            }
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, rhs: &Packet) -> std::cmp::Ordering {
        self.partial_cmp(rhs)
            .expect("Total ordering returned None.")
    }
}

impl Packet {
    fn singleton(v: u32) -> Packet {
        Packet::List(vec![Packet::Literal(v)])
    }

    /// Read a single packet from input and filter empty lines.
    pub fn read_line(line: String) -> Option<Packet> {
        if line.is_empty() {
            None
        } else {
            Packet::read_packets(line.chars().take_within("[]"))
        }
    }

    /// Anamorphism to repeatedly apply parse until all input is consumed.
    pub fn read_packets(line: String) -> Option<Packet> {
        Some(Packet::List(
            itertools::unfold(&mut line.chars().peekable(), |i| {
                let packet = Packet::parse(i)?;
                Some(packet)
            })
            .collect(),
        ))
    }

    // Consumes part of the input to parse a number or sublist, and returns result × remaing input.
    fn parse(line_iter: &mut ParseInput) -> Option<Packet> {
        match line_iter.peek()? {
            // Match a sublist using take_within() to find scope.
            '[' => Some(Packet::read_packets(line_iter.take_within("[]"))?),

            // Parse a number literal.
            i if i.is_numeric() => Some(Packet::Literal(
                line_iter
                    .peeking_take_while(|c| c.is_ascii_digit())
                    .collect::<String>()
                    .parse()
                    .ok()?,
            )),

            // Consume and ignore commas.
            ',' => {
                line_iter.next();
                Packet::parse(line_iter)
            }

            c => panic!("Invalid input character: {:?}", c),
        }
    }
}

pub fn read_data(lines: PuzzleInput) -> Signal {
    lines
        .into_iter()
        .filter_map(Packet::read_line)
        .collect::<Vec<_>>()
}

pub fn part1(signal: &Signal) -> usize {
    signal
        .iter()
        .chunks(2)
        .into_iter()
        .map(|mut x| {
            (
                x.next().expect("Missing first packet in pair."),
                x.next().expect("Missing second packet in pair."),
            )
        })
        .enumerate()
        .filter_map(|(i, (x, y))| if x < y { Some(i + 1) } else { None })
        .sum()
}

pub fn part2(signal: &Signal) -> usize {
    let dividers = [Packet::singleton(2), Packet::singleton(6)];
    let sorted = signal
        .iter()
        .chain(dividers.iter())
        .sorted()
        .collect::<Vec<_>>();
    dividers
        .clone()
        .iter()
        .map(|d| {
            1 + sorted
                .binary_search(&d)
                .expect("Could not find dividers after sorting.")
        })
        .product()
}

pub fn main() {
    let _ = run(
        13,
        read_data,
        part1,
        part2,
        [Some(13), Some(4643), Some(140), Some(21614)],
    );
}
