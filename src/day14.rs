use crate::prelude::{run, Ascii2d, Coord, PrintAscii2d, PuzzleInput};
use itertools::Itertools;
use std::collections::HashMap;
use std::str::FromStr;

/// A cell can be filled with Rock or Sand.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cell {
    Rock,
    Sand,
}

impl Ascii2d for Cell {
    fn to_char(cell: Option<&Cell>) -> char {
        match cell {
            Some(Cell::Rock) => '#',
            Some(Cell::Sand) => 'o',
            None => ' ',
        }
    }
}

type Mapping = HashMap<Coord, Cell>;

/// Map contains cells of rock/sand, as well as boundary information.
#[derive(Clone)]
pub struct Map {
    cells: Mapping,
    top: i32,
    bottom: i32,
    left: i32,
    right: i32,
}

impl Map {
    /// Origin of all the sand.
    const SOURCE: Coord = Coord::new(500, 0);

    /// Read input file to produce initial map state.
    pub fn from(lines: Vec<String>) -> Map {
        let mut map = Map {
            cells: Mapping::new(),
            left: i32::MAX,
            right: i32::MIN,
            top: i32::MAX,
            bottom: i32::MIN,
        };
        lines.into_iter().for_each(|line| {
            line.split_ascii_whitespace()
                .step_by(2) // Skip arrows.
                .map(|coord: &str| {
                    // Extract coordinates and keep track of boundaries.
                    let mut xy = coord.split(',');
                    let x = i32::from_str(xy.next().expect("No x coordinate!"))
                        .expect("Cannot read x coordinate!");
                    let y = i32::from_str(xy.next().expect("No y coordinate!"))
                        .expect("Cannot read y coordinate!");

                    map.left = map.left.min(x);
                    map.right = map.right.max(x);
                    map.top = map.top.min(y);
                    map.bottom = map.bottom.max(y);
                    Coord::new(x, y)
                })
                .collect::<Vec<_>>() // Collect intermediary results to free mutable borrows.
                .iter()
                .tuple_windows() // Sliding window over each pair of corner points.
                .for_each(|(a, b)| a.span(b).for_each(|c| map.spawn_rock(c)));
        });
        map
    }

    /// Check whether a cell contains rock/sand.
    pub fn empty(&self, c: Coord) -> bool {
        self.cells.get(&c).is_none()
    }

    pub fn print(&self) {
        self.cells
            .print_map(self.top, self.bottom, self.left, self.right);
    }

    /// Spawn a unit of sand at the source, and see where it lands. Returns true if sand added,
    /// false if not (either because it disappeared into oblivion, or because the soruce is stuck).
    pub fn spawn(&mut self) -> bool {
        let mut location = Map::SOURCE;
        let mut previous = None;

        while previous != Some(location) {
            if location.y() >= self.bottom || !self.empty(Map::SOURCE) {
                return false;
            }

            let _ = previous.insert(location);

            // Try fall directions in order.
            location = if self.empty(location.fall()) {
                location.fall()
            } else if self.empty(location.fall_left()) {
                location.fall_left()
            } else if self.empty(location.fall_right()) {
                location.fall_right()
            } else {
                // Stay put.
                location
            };
        }

        self.cells.insert(location, Cell::Sand);
        true
    }

    /// Flood map and count sand added.
    pub fn flood(&mut self) -> u32 {
        let mut sand = 0;

        while self.spawn() {
            sand += 1;
        }
        sand
    }

    /// Insert rock in given location.
    pub fn spawn_rock(&mut self, c: Coord) {
        self.cells.insert(c, Cell::Rock);
    }

    /// Build a floor at bottom level, stretching from left to right.
    pub fn build_floor(&mut self) {
        Coord::hline(self.bottom, self.left, self.right).for_each(|c| self.spawn_rock(c));
    }
}

pub fn read_data(lines: PuzzleInput) -> Map {
    Map::from(lines)
}

pub fn part1(m: &Map) -> u32 {
    let mut map = m.to_owned();

    map.print();
    let sand = map.flood();
    map.print();

    sand
}

pub fn part2(m: &Map) -> u32 {
    let mut map = m.to_owned();

    // Define floor level.
    map.bottom += 2;

    // Accomodate more sand.
    map.left -= 148;
    map.right += 102;

    // Add the relevant rock cells.
    map.build_floor();

    // Run the experiment again.
    map.print();
    let sand = map.flood();
    map.print();

    sand
}

pub fn main() {
    let _ = run(
        14,
        read_data,
        part1,
        part2,
        [Some(24), Some(885), Some(93), Some(28691)],
    );
}
