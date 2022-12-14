use crate::prelude::{run, PuzzleInput};
use itertools::Itertools;
use std::collections::HashMap;
use std::str::FromStr;

/// A cell can be filled with Rock or Sand.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cell {
    Rock,
    Sand,
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
    const SOURCE: Coord = Coord { x: 500, y: 0 };

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
                    let x = i32::from_str(xy.next().unwrap()).unwrap();
                    let y = i32::from_str(xy.next().unwrap()).unwrap();

                    if x < map.left {
                        map.left = x;
                    }
                    if x > map.right {
                        map.right = x;
                    }
                    if y < map.top {
                        map.top = y;
                    }
                    if y > map.bottom {
                        map.bottom = y;
                    }
                    Coord { x, y }
                })
                .collect::<Vec<_>>() // Collect intermediary results to free mutable borrows.
                .iter()
                .tuple_windows() // Sliding window over each pair of corner points.
                .for_each(|(a, b)| {
                    if a.x == b.x {
                        // Rows.
                        (a.y.min(b.y)..=a.y.max(b.y)).for_each(|y| {
                            map.spawn_rock(Coord { x: a.x, y });
                        });
                    } else if a.y == b.y {
                        // Columns.
                        (a.x.min(b.x)..=a.x.max(b.x)).for_each(|x| {
                            map.spawn_rock(Coord { x, y: a.y });
                        });
                    } else {
                        panic!("Unexpected formation: {:?} -> {:?}", a, b)
                    }
                });
        });
        map
    }

    /// Check whether a cell contains rock/sand.
    pub fn empty(&self, c: Coord) -> bool {
        self.cells.get(&c).is_none()
    }

    /// Printing enabled using a feature, otherwise treat as no-op.
    pub fn print(&self) {
        #[cfg(feature = "print")]
        {
            (0..=self.bottom).for_each(|y| {
                (self.left..=self.right).for_each(|x| match &self.cells.get(&Coord { x, y }) {
                    Some(Cell::Rock) => print!("#"),
                    Some(Cell::Sand) => print!("o"),
                    None => print!(" "),
                });
                println!();
            });
            println!();
        }
    }

    /// Spawn a unit of sand at the source, and see where it lands. Returns true if sand added,
    /// false if not (either because it disappeared into oblivion, or because the soruce is stuck).
    pub fn spawn(&mut self) -> bool {
        let mut location = Map::SOURCE;
        let mut previous = None;

        while previous != Some(location) {
            if location.y >= self.bottom || !self.empty(Map::SOURCE) {
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

    pub fn spawn_rock(&mut self, c: Coord) {
        self.cells.insert(c, Cell::Rock);
    }

    pub fn build_floor(&mut self) {
        (self.left..=self.right).for_each(|x| self.spawn_rock(Coord { x, y: self.bottom }));
    }
}

/// 2D cartesian coordinate, origin at top-left.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn translate(self, delta_x: i32, delta_y: i32) -> Coord {
        Coord {
            x: self.x + delta_x,
            y: self.y + delta_y,
        }
    }
    pub fn fall(self) -> Coord {
        self.translate(0, 1)
    }

    pub fn fall_left(self) -> Coord {
        self.translate(-1, 1)
    }

    pub fn fall_right(self) -> Coord {
        self.translate(1, 1)
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
