use crate::prelude::{run, Coord, ExtraIterators, PuzzleInput};
use itertools::Itertools;
use std::iter::Iterator;

type Intermediate = String;
type Line = [bool; 7];

#[derive(Debug)]
pub struct Shape {
    pixels: Vec<Coord>,
}

impl Shape {
    pub fn move_to(&mut self, dir: Direction, lines: &[Line]) {
        match dir {
            Direction::Left => self.left(lines),
            Direction::Right => self.right(lines),
        };
    }

    pub fn down(&mut self, lines: &[Line]) -> bool {
        if self.pixels.iter().any(|p| {
            if p.y() == 0 {
                true
            } else {
                let new_pos = p.up();
                let x: usize = new_pos.x_as_usize();
                let y: usize = new_pos.y_as_usize();
                lines.get(y).map_or(false, |row| row[x])
            }
        }) {
            return false;
        }

        self.pixels.iter_mut().for_each(|p| *p = p.up());
        true
    }
    pub fn left(&mut self, lines: &[Line]) {
        if !self.pixels.iter().any(|p| {
            if p.x() == 0 {
                true
            } else {
                let new_pos = p.left();
                let x: usize = new_pos.x_as_usize();
                let y: usize = new_pos.y_as_usize();
                lines.get(y).map_or(false, |row| row[x])
            }
        }) {
            self.pixels.iter_mut().for_each(|p| *p = p.left());
        }
    }
    pub fn right(&mut self, lines: &[Line]) {
        if !self.pixels.iter().any(|p| {
            if p.x() == 6 {
                true
            } else {
                let new_pos = p.right();
                let x: usize = new_pos.x_as_usize();
                let y: usize = new_pos.y_as_usize();
                lines.get(y).map_or(false, |row| row[x])
            }
        }) {
            self.pixels.iter_mut().for_each(|p| *p = p.right());
        }
    }
}

#[derive(Debug, Default)]
pub struct ShapeIterator {
    idx: u8,
    height: i32,
}

impl ShapeIterator {
    pub fn set_top(&mut self, top: usize) {
        self.height = top as i32;
    }

    pub fn reset(&mut self) {
        self.idx = 0;
    }
}

impl Iterator for ShapeIterator {
    type Item = Shape;

    fn next(&mut self) -> Option<Shape> {
        let pixels = match self.idx {
            0 => vec![
                Coord::new(2, 3 + self.height),
                Coord::new(3, 3 + self.height),
                Coord::new(4, 3 + self.height),
                Coord::new(5, 3 + self.height),
            ],
            1 => vec![
                Coord::new(3, 3 + self.height),
                Coord::new(2, 3 + self.height + 1),
                Coord::new(3, 3 + self.height + 1),
                Coord::new(4, 3 + self.height + 1),
                Coord::new(3, 3 + self.height + 2),
            ],
            2 => vec![
                Coord::new(2, 3 + self.height),
                Coord::new(3, 3 + self.height),
                Coord::new(4, 3 + self.height),
                Coord::new(4, 3 + self.height + 1),
                Coord::new(4, 3 + self.height + 2),
            ],
            3 => vec![
                Coord::new(2, 3 + self.height),
                Coord::new(2, 3 + self.height + 1),
                Coord::new(2, 3 + self.height + 2),
                Coord::new(2, 3 + self.height + 3),
            ],
            4 => vec![
                Coord::new(2, 3 + self.height),
                Coord::new(3, 3 + self.height),
                Coord::new(2, 3 + self.height + 1),
                Coord::new(3, 3 + self.height + 1),
            ],
            _ => panic!(),
        };
        self.idx = (self.idx + 1) % 5;
        Some(Shape { pixels })
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    Left,
    Right,
}

impl Direction {
    pub fn from(c: char) -> Direction {
        match c {
            '<' => Direction::Left,
            '>' => Direction::Right,
            _ => panic!("Invalid input"),
        }
    }
}

#[derive(Debug)]
pub struct InputIterator {
    queue: Vec<Direction>,
    idx: usize,
}

impl InputIterator {
    pub fn new(input: String) -> InputIterator {
        let queue = input.chars().map(Direction::from).collect();
        InputIterator { idx: 0, queue }
    }

    pub fn reset(&mut self) {
        self.idx = 0;
    }
}

impl Iterator for InputIterator {
    type Item = Direction;

    fn next(&mut self) -> Option<Direction> {
        let dir = self.queue[self.idx];
        self.idx = (self.idx + 1) % self.queue.len();
        Some(dir)
    }
}

#[derive(Debug)]
struct Level {
    lines: Vec<Line>,
    shapes: ShapeIterator,
    wind: InputIterator,
}

impl Level {
    pub fn reset(&mut self) {
        self.lines = Vec::new();
        self.shapes.reset();
        self.wind.reset();
    }

    pub fn spawn(&mut self, print: bool) {
        self.shapes.set_top(self.lines.len());
        let mut shape = self.shapes.next().expect("Infinite stream ran out!");

        let mut falling = true;

        if print {
            self.print(Some(&shape));
        }

        while falling {
            let wind = self.wind.next().expect("No wind!");

            shape.move_to(wind, &self.lines);
            if print {
                self.print(Some(&shape));
            }

            falling = shape.down(&self.lines);
            if print {
                self.print(Some(&shape));
            }
        }

        shape.pixels.into_iter().for_each(|p| {
            let x: usize = p.x_as_usize();
            let y: usize = p.y_as_usize();

            while self.lines.len() <= y {
                self.lines.push([false; 7]);
            }

            if self.lines[y][x] {
                panic!("Missed collision");
            }
            self.lines[y][x] = true;
        });

        if print {
            self.print(None);
        }
    }
}

impl Level {
    fn new(shapes: ShapeIterator, wind: InputIterator) -> Level {
        Level {
            lines: Vec::new(),
            shapes,
            wind,
        }
    }
    fn print(&self, shape: Option<&Shape>) {
        for y in (0..4 + self.lines.len()).rev() {
            print!("|");
            for x in 0..8 {
                if x == 7 {
                    print!("|");
                } else if self.lines.get(y).map_or(false, |row| row[x]) {
                    print!("#");
                } else if shape.is_some_and(|s| s.pixels.contains(&Coord::new(x as i32, y as i32)))
                {
                    print!("@");
                } else {
                    print!(".");
                }
            }
            println!();
        }
        println!("+-------+");
    }
}

pub fn read_data(lines: PuzzleInput) -> Intermediate {
    lines[0].clone()
}

pub fn part1(input: &Intermediate) -> usize {
    let shapes = ShapeIterator::default();
    let winds = InputIterator::new(input.clone());
    let mut level = Level::new(shapes, winds);

    (0..2022).for_each(|_| level.spawn(false));

    level.lines.len()
}

pub fn part2(input: &Intermediate) -> usize {
    let shapes = ShapeIterator::default();
    let winds = InputIterator::new(input.clone());

    let mut level = Level::new(shapes, winds);

    let extrapolate_from = (0..20000) // Run 20000 steps to search for cycles.
        .map(|_| {
            level.spawn(false);
            level.lines.len()
        })
        .collect::<Vec<_>>();

    let differential = extrapolate_from // Get the height different after each step.
        .iter()
        .tuple_windows()
        .map(|(a, b)| b - a)
        .collect::<Vec<_>>();

    let period = (1..20000) // Compare list with delayed list to find cycles.
        .map(|n| {
            let cycle = differential
                .clone()
                .into_iter()
                .skip(n)
                .zip(differential.clone().into_iter())
                .take_while(|(a, b)| a == b)
                .count();
            (n, cycle)
        })
        .sorted_by_key(|(_, c)| *c) // Sort by cycle length.
        .map(|(n, _)| n) // Extract starting points.
        .rev()
        .take(10) // Take 10 largest cycles.
        .tuple_windows()
        .map(|(a, b)| a - b) // Subtract consecutive items to get the period.
        .tuple_windows()
        .map(|(a, b)| a + b) // Combine pairwise cycles, somehow finds subcycles otherwise.
        .singular() // Check all are the same.
        .unwrap_or(1);

    let start = 1_000_000_000_000 % period; // Where do we need to start to end op on 1 trillion?

    level.reset();

    (0..start).for_each(|_| {
        level.spawn(false);
    });

    let at_start = level.lines.len(); // Determine value at start.

    (0..period).for_each(|_| {
        level.spawn(false);
    });

    let after_cycle = level.lines.len(); // Determine gain after single cycle.

    let cycle_gain = after_cycle - at_start; // Calculate increment per cycle.
    let cycles = 1_000_000_000_000 / period; // How many cycles to simualte.

    at_start + cycle_gain * cycles
}

pub fn main() {
    let _ = run(
        17,
        read_data,
        part1,
        part2,
        [
            Some(3068),
            Some(3168),
            Some(1514285714288),
            Some(1554117647070),
        ],
    );
}
