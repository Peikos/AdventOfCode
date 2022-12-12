use crate::prelude::{run, PuzzleInput};
use pathfinding::directed::astar::astar;
use std::collections::HashMap;

type Coord = (usize, usize);
type Height = u8;
type Mapping = HashMap<Coord, Height>;

pub struct Map {
    elevations: Mapping,
    start: Coord,
    end: Coord,
}

impl Map {
    fn from(lines: Vec<String>) -> Map {
        let mut elevations: Mapping = HashMap::new();
        let mut start: Coord = (0, 0);
        let mut end: Coord = (0, 0);

        lines.into_iter().enumerate().for_each(|(y, row)| {
            row.bytes().enumerate().for_each(|(x, c)| {
                elevations.insert(
                    (x, y),
                    if c == 83 {
                        // Ascii S
                        start = (x, y);
                        0
                    } else if c == 69 {
                        // Ascii E
                        end = (x, y);
                        25
                    } else if (97..123).contains(&c) {
                        // Ascii a-z
                        c - 97
                    } else {
                        panic!("Invalid character in input!")
                    },
                );
            })
        });

        Map {
            elevations,
            start,
            end,
        }
    }

    fn find_paths(&self, startpoints: Vec<Coord>) -> u32 {
        startpoints
            .into_iter()
            .filter_map(|start| {
                // Perform a* on each (potential) startpoint
                astar(
                    &start, // Our starting point.
                    |(x, y)| {
                        // Get paths from node - all nodes with Chebyshev distance 1
                        let neighbours = [
                            // Possible neighbours of cell
                            x.checked_add(1).map(|xn| (xn, *y)),
                            x.checked_sub(1).map(|xn| (xn, *y)),
                            y.checked_add(1).map(|yn| (*x, yn)),
                            y.checked_sub(1).map(|yn| (*x, yn)),
                        ];
                        neighbours
                            .into_iter()
                            .filter_map(|c: Option<Coord>| {
                                // Filter out unreachable neighbours
                                if self.elevations.contains_key(&c?) {
                                    let there = *self.elevations.get(&c?)?;
                                    let here = *self.elevations.get(&(*x, *y))?;
                                    if there <= here + 1 {
                                        Some((c?, 1))
                                    } else {
                                        None // Nope, height difference.
                                    }
                                } else {
                                    None // Nope, off the map.
                                }
                            })
                            .collect::<Vec<_>>()
                    },
                    |_| 1,              // Constant cost if node is accessible.
                    |c| c == &self.end, // Our target.
                )
            })
            .map(|x| x.1) // We only care about total path length.
            .min() // Choose shortest path.
            .expect("No shortest trail!")
    }
}

pub fn read_data(lines: PuzzleInput) -> Map {
    Map::from(lines)
}

pub fn part1(map: &Map) -> u32 {
    map.find_paths(vec![map.start])
}

pub fn part2(map: &Map) -> u32 {
    map.find_paths(
        map.elevations // Possible start points: elevation 0.
            .iter()
            .filter_map(|(key, &val)| if val == 0 { Some(*key) } else { None })
            .collect(),
    )
}

pub fn main() {
    let _ = run(
        12,
        read_data,
        part1,
        part2,
        [Some(31), Some(490), Some(29), Some(488)],
    );
}
