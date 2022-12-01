#![allow(non_snake_case)]

use inquire::MultiSelect;

fn run_day(day: &u32) {
    println!("DAY {:?}", day);
    match day {
        1 => aoc::day1::main(),
        2 => aoc::day2::main(),
        3 => aoc::day3::main(),
        4 => aoc::day4::main(),
        5 => aoc::day5::main(),
        6 => aoc::day6::main(),
        7 => aoc::day7::main(),
        8 => aoc::day8::main(),
        9 => aoc::day9::main(),
        10 => aoc::day10::main(),
        11 => aoc::day11::main(),
        12 => aoc::day12::main(),
        13 => aoc::day13::main(),
        14 => aoc::day14::main(),
        15 => aoc::day15::main(),
        16 => aoc::day16::main(),
        17 => aoc::day17::main(),
        18 => aoc::day18::main(),
        19 => aoc::day19::main(),
        20 => aoc::day20::main(),
        21 => aoc::day21::main(),
        22 => aoc::day22::main(),
        23 => aoc::day23::main(),
        24 => aoc::day24::main(),
        25 => aoc::day25::main(),
        _ => panic!("No such day."),
    }
}

fn main() {
    if std::env::args().len() > 1 {
        std::env::args()
            .skip(1)
            .for_each(|d| run_day(&d.parse::<u32>().unwrap_or(0)));
    } else {
        MultiSelect::new("SELECT DAY", (1..26).collect())
            .prompt()
            .expect("No result")
            .iter()
            .for_each(run_day);
    }
}
