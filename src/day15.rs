use crate::prelude::{run, Circle, Coord, Manhattan, PuzzleInput};
use itertools::Itertools;
use std::time::Instant;

type SensorData = (Vec<Circle<Manhattan>>, Vec<Coord>);

pub fn read_data(lines: PuzzleInput) -> SensorData {
    lines
        .into_iter()
        .filter_map(|line| {
            let mut parts = line.split('=').skip(1).map(|part| {
                part.chars()
                    .filter(|c| *c == '-' || c.is_ascii_digit())
                    .collect::<String>()
                    .parse()
                    .expect("No parse for coordinate part.")
            });

            let c = Coord::new(parts.next()?, parts.next()?);
            let b = Coord::new(parts.next()?, parts.next()?);
            let r = c.manhattan(b);
            Some((Circle::manhattan(c, r), b))
        })
        .unzip()
}

pub fn part1((sensor_ranges, beacons): &SensorData) -> u128 {
    let sensor_ranges = sensor_ranges
        .clone()
        .iter()
        .cloned()
        .sorted_by_key(|s| std::cmp::Reverse(s.radius()))
        .collect::<Vec<_>>();

    let y = if beacons.len() == 14 { 10 } else { 2_000_000 };

    let west = sensor_ranges
        .iter()
        .filter(|c| c.covers_y(y))
        .map(|c| c.westmost().x())
        .min()
        .expect("No eastmost sensor range.");

    let east = sensor_ranges
        .iter()
        .filter(|c| c.covers_y(y))
        .map(|c| c.eastmost().x())
        .max()
        .expect("No westmost sensor range.");

    let start = Instant::now();

    let f = Coord::hline(y, west, east)
        .filter(|p| sensor_ranges.iter().any(|c| c.clone().contains(p)) && !beacons.contains(p))
        .count() as u128;

    let duration = start.elapsed();
    println!("Line {:?}", duration);

    f
}

pub fn part2((sensor_ranges, _beacons): &SensorData) -> u128 {
    let bounds = if sensor_ranges.len() == 14 {
        20
    } else {
        4_000_000
    };

    let start = Instant::now();
    let f = sensor_ranges
        .iter()
        .map(|c| {
            c.grow()
                .boundary()
                .into_iter()
                .filter(|c| {
                    let x = c.x();
                    let y = c.y();
                    x >= 0 && x <= bounds && y >= 0 && y <= bounds
                })
                .collect::<Vec<_>>()
        })
        .concat()
        .iter()
        .find(|p| sensor_ranges.iter().all(|c| !c.clone().contains(p)))
        .map_or(0, |c| 4_000_000 * c.x() as u128 + c.y() as u128);
    let duration = start.elapsed();
    println!("Boundaries {:?}", duration);
    f
}

pub fn main() {
    let _ = run(
        15,
        read_data,
        part1,
        part2,
        [
            Some(26),
            Some(4_737_443),
            Some(56_000_011),
            Some(11_482_462_818_989),
        ],
    );
}
