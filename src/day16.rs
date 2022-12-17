use crate::pest::Parser;
use crate::prelude::{run, PuzzleInput};
use itertools::Itertools;
use pathfinding::directed::astar::astar;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "day16.pest"]
struct LocationParser;

#[derive(Clone, Debug)]
pub struct Location {
    flow_rate: usize,
    tunnels: Vec<String>,
}

impl Location {
    pub fn from(line: &str) -> (String, Location) {
        let mut res = LocationParser::parse(Rule::location, line)
            .expect("Parse failed!")
            .next()
            .expect("Parse empty!")
            .into_inner();

        (
            res.next()
                .expect("No name found in parse!")
                .as_str()
                .to_string(),
            Location {
                flow_rate: res
                    .next()
                    .expect("No valid flow rate!")
                    .as_str()
                    .parse()
                    .expect("Flow rate is not a valid number!"),
                tunnels: res
                    .next()
                    .expect("Cannot parse tunnels!")
                    .into_inner()
                    .into_iter()
                    .map(|item| item.as_str().to_string())
                    .collect(),
            },
        )
    }
}

/// Preparational phase yields some immutable structs that are carried around packaged here.
pub struct GlobalConst {
    distance_matrix: DistanceMatrix,
    location_map: LocationMap,
    relevant_locations: Vec<String>,
}

type LocationMap = HashMap<String, Location>;
type DistanceMatrix = HashMap<(String, String), usize>;

/// Parse the input file.
pub fn read_data(lines: PuzzleInput) -> GlobalConst {
    // Store location data in hashmap.
    let mut location_map: LocationMap = HashMap::new();
    lines.iter().for_each(|line| {
        let (name, loc) = Location::from(line);
        location_map.insert(name, loc);
    });

    // Calculate the distance between each relevant pair of relevant points.
    let mut distance_matrix: DistanceMatrix = HashMap::new();

    #[rustfmt::skip]
    location_map
        .keys()
        .cartesian_product(location_map.keys())
        .for_each(|(from, to)| {
            if from < to &&                            // Symmetric, only calculate half.
               location_map
                .get(to)
                .is_some_and(|loc| loc.flow_rate > 0)  // Disregard nodes without valve.
            {
                // Calculate shortest route between two valves.
                let time_to_start = astar(
                    &from,
                    |&start| {
                        location_map.get(start).map_or(Vec::new(), |location| {
                            location.tunnels.iter().map(|tunnel| (tunnel, 1)).collect()
                        })
                    },
                    |_| 1,
                    |end| end == &to,
                )
                .expect("Could not find a shortest route!")
                .1 + 1; // +1 to account for starting the valve

                // Symmetric matrix, calculate once and fill twice.
                distance_matrix.insert((from.clone(), to.clone()), time_to_start);
                distance_matrix.insert((to.clone(), from.clone()), time_to_start);
            }
        });

    // Vec of keys to pass around for keeping track of unvisited valves.
    let relevant_locations = location_map
        .keys()
        .filter(|&loc| location_map.get(loc).is_some_and(|loc| loc.flow_rate > 0))
        .cloned()
        .collect::<Vec<String>>();

    GlobalConst {
        distance_matrix,
        location_map,
        relevant_locations,
    }
}

/// Recursively determine score for each possible route.
fn explore_next_step(
    global: &GlobalConst,
    here: String,
    time: usize,
    score: usize,
    next_valve_choices: Vec<String>,
    path: Vec<String>,
) -> usize {
    next_valve_choices
        .iter()
        .map(|choice| {
            let dist = global
                .distance_matrix
                .get(&(here.clone(), choice.to_string()))
                .expect("No distance known between locations.");
            let new_time = time.saturating_sub(*dist);

            if new_time == 0 {
                // Stop exploring when time is up.
                return 0;
            }

            let flow_rate = global
                .location_map
                .get(&choice.clone())
                .expect("Can't find location flow rate!")
                .flow_rate;

            // Recurse one step further.
            explore_next_step(
                global,
                choice.to_string(),
                new_time,
                new_time * flow_rate,
                next_valve_choices
                    .iter()
                    .cloned()
                    .filter(|c| c != choice)
                    .collect(),
                crate::prelude::cons(path.clone(), choice.to_string()),
            )
        })
        .max()
        .map_or(score, |s| s + score)
}

/// Start recursive function with default initial values.
pub fn explore(global: &GlobalConst, work_division: Vec<String>, time_available: usize) -> usize {
    explore_next_step(
        global,
        "AA".to_string(),
        time_available,
        0,
        work_division,
        Vec::new(),
    )
}

pub fn part1(global: &GlobalConst) -> usize {
    explore(global, global.relevant_locations.clone(), 30)
}

pub fn part2(global: &GlobalConst) -> usize {
    // Turns out dividing into approximately equal parts contains an optimal solution for both
    // sets. In order to explore further, we can use a range starting at half the length of the
    // global.relevant_locations vector and map over that, combining results using max.
    let n = global.relevant_locations.len() / 2;

    // Calculate all possible divisions between me and the elephant.
    global
        .relevant_locations
        .iter()
        .cloned()
        .combinations(n) // Take n for me.
        .map(|my_part| {
            let elephant_part = global // Leave the rest for the elephant.
                .relevant_locations
                .iter()
                .cloned()
                .filter(|item| !my_part.contains(item))
                .collect();

            let me = explore(global, my_part, 26);
            let elephant = explore(global, elephant_part, 26);

            me + elephant
        })
        .max()
        .unwrap_or(0)
}

pub fn main() {
    let _ = run(
        16,
        read_data,
        part1,
        part2,
        [Some(1651), Some(1940), Some(1707), Some(2469)],
    );
}
