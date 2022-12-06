use crate::prelude::{run, PuzzleInput};
use std::collections::HashSet;

type DataStream = Vec<char>;
pub struct DataStreams(Vec<DataStream>); // Process multiple examples as one in order to preserve single Test entry.

/// Not much to do, just prepare by splitting in characters.
pub fn read_data(lines: PuzzleInput) -> DataStreams {
    DataStreams(
        lines
            .map(|l| {
                if let Ok(ll) = l {
                    ll.chars().collect()
                } else {
                    panic!()
                }
            })
            .collect(),
    )
}

impl DataStreams {
    /// Check a datastream to determine start of packet.
    fn check_datastream(datastream: &DataStream, size: u32) -> u32 {
        size + datastream // Add size to offset starting index to end of sequence.
            .windows(size as usize)
            .enumerate()
            .filter_map(|(i, w)| {
                let mut uniq = HashSet::new();
                // Sequence contains no repeated characters if each can be added to the same HashSet.
                if w.into_iter().all(move |x| uniq.insert(x)) {
                    Some(i) // Return starting index.
                } else {
                    None
                }
            })
            .next() // Only care about first occurence.
            .expect("No marker found after reaching end.") as u32
    }

    /// Check multiple datastreams.
    fn check_windows(&self, size: u32) -> Vec<u32> {
        self.0
            .iter()
            .map(|ds| DataStreams::check_datastream(ds, size))
            .collect()
    }
}

pub fn part1(input: &DataStreams) -> Vec<u32> {
    input.check_windows(4)
}

pub fn part2(input: &DataStreams) -> Vec<u32> {
    input.check_windows(14)
}

pub fn main() {
    let _ = run(
        6,
        read_data,
        part1,
        part2,
        [
            Some(vec![7, 5, 6, 10, 11]),
            Some(vec![1287]),
            Some(vec![19, 23, 23, 29, 26]),
            Some(vec![3716]),
        ],
    );
}
