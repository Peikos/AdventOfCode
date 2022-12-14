use itertools::Itertools;
use std::cmp::{Ordering, Ordering::*};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;
use std::ops::{Add, Sub};
use std::path::PathBuf;
use std::time::Instant;

pub enum Data {
    Test,
    Input,
}

pub type PuzzleInput = Vec<String>;

impl Data {
    pub fn load_input(&self, day: u8) -> PuzzleInput {
        let mut path: PathBuf = ["data"].iter().collect();
        path.push(day.to_string());
        path.push(match self {
            Data::Test => "test",
            Data::Input => "input",
        });

        let file = File::open(&path).unwrap_or_else(|_| panic!("File not found: {:?}!", path));
        let buf_reader = BufReader::new(file);
        buf_reader
            .lines()
            .collect::<Result<Vec<_>, _>>()
            .expect("Failed to load puzzle input.")
    }
}

#[cfg(not(feature = "timing"))]
pub fn run<I, R, Pre, F1, F2>(
    day: u8,
    preprocess: Pre,
    part1: F1,
    part2: F2,
    outputs: [Option<R>; 4],
) -> Result<(), String>
where
    Pre: Fn(PuzzleInput) -> I,
    F1: Fn(&I) -> R,
    F2: Fn(&I) -> R,
    R: Debug + PartialEq + Eq + Clone,
{
    println!("DAY {:?}", day);

    if cfg!(feature = "test") {
        let test = preprocess(Data::Test.load_input(day));
        let results = [part1(&test), part2(&test)];
        dbg!(&results);
        std::process::exit(0);
    }

    let test = preprocess(Data::Test.load_input(day));
    let data = preprocess(Data::Input.load_input(day));

    let start = Instant::now();
    let results = [part1(&test), part1(&data), part2(&test), part2(&data)];

    results
        .into_iter()
        .zip(outputs)
        .for_each(|(result, output)| {
            if let Some(expected) = output {
                assert_eq!(result, expected);
            } else {
                println!("{:?}", result);
            }
        });
    let duration = start.elapsed();
    println!("{:?}", duration);
    Ok(())
}

#[cfg(feature = "timing")]
pub fn run<I, R, Pre, F1, F2>(
    day: u8,
    preprocess: Pre,
    part1: F1,
    part2: F2,
    _outputs: [Option<R>; 4],
) -> Result<(), String>
where
    Pre: Fn(PuzzleInput) -> I,
    F1: Fn(&I) -> R,
    F2: Fn(&I) -> R,
    R: Debug + PartialEq + Eq,
{
    let data = preprocess(Data::Input.load_input(day));

    (0..100).for_each(|_| {
        _ = part1(&data);
        _ = part2(&data);
    });

    let start = Instant::now();
    (0..1000).for_each(|_| {
        _ = part1(&data);
    });
    let duration1 = start.elapsed();

    let start = Instant::now();
    (0..1000).for_each(|_| {
        _ = part2(&data);
    });
    let duration2 = start.elapsed();

    println!("??? DAY {: >2} ???", day);
    println!("?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????");
    println!(
        "??? part 1: {: >12?} ??? part 2: {: >12?} ???",
        duration1 / 1000,
        duration2 / 1000
    );
    println!("?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????");
    println!("");
    Ok(())
}

pub struct MapAccum<I, Acc, F> {
    accu: Acc,
    iter: I,
    f: F,
}

impl<I, Acc, F> MapAccum<I, Acc, F> {
    pub fn new(iter: I, accu: Acc, f: F) -> Self {
        Self { iter, accu, f }
    }
}

impl<I, B, Acc, F> Iterator for MapAccum<I, Acc, F>
where
    I: Iterator,
    F: FnMut(&Acc, I::Item) -> Option<(Acc, B)>,
{
    type Item = B;

    fn next(&mut self) -> Option<B> {
        let (new_acc, res) = (self.f)(&self.accu, self.iter.next()?)?;
        self.accu = new_acc;
        Some(res)
    }
}

pub trait ExtraIterators: Iterator {
    fn take_within(&mut self, delim: &str) -> String
    where
        Self: Sized + Iterator<Item = char>,
    {
        let mut nesting = 0;

        // Match opening delimiter, or panic.
        assert_eq!(
            self.next(),
            Some(delim.chars().next().expect("Missing delimiters!")),
            "Expected opening delimiter did not match input."
        );

        self.by_ref()
            .take_while(|&c| {
                nesting += delim
                    .chars()
                    .position(|delim| c == delim)
                    .map_or(0, |idx| [1, -1][idx]);
                nesting >= 0
            })
            .collect::<String>()
    }

    fn map_accum<Acc, F, B>(self, accu: Acc, f: F) -> MapAccum<Self, Acc, F>
    where
        Self: Sized,
        F: FnMut(&Acc, Self::Item) -> Option<(Acc, B)>,
    {
        MapAccum::new(self, accu, f)
    }

    fn singular(&mut self) -> Option<Self::Item>
    where
        Self::Item: Eq,
    {
        let guess = self.next();

        self.fold(guess, |u, t| if u == Some(t) { u } else { None })
    }
}

impl<T: ?Sized> ExtraIterators for T where T: Iterator {}

pub fn fst<A, B, C, F>(func: F, (a, b): (A, B)) -> (C, B)
where
    F: Fn(A) -> C,
{
    (func(a), b)
}

pub fn snd<A, B, C, F>(func: F, (a, b): (A, B)) -> (A, C)
where
    F: Fn(B) -> C,
{
    (a, func(b))
}

pub fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}

pub fn cons<A>(vec: Vec<A>, item: A) -> Vec<A> {
    let mut vec = vec;
    vec.push(item);
    vec
}

pub fn print_display(display: Vec<Vec<bool>>) {
    display.into_iter().for_each(|row| {
        println!(
            "{}",
            row.into_iter()
                .map(|c| if c { '#' } else { '.' })
                .collect::<String>()
        )
    });
    println!();
}

pub fn lookup_char(hash: usize) -> char {
    match hash {
        311928102 => 'A',
        244620583 => 'B',
        210797862 => 'C',
        244622631 => 'D',
        504413231 => 'E',
        34651183 => 'F',
        211191078 => 'G',
        311737641 => 'H',
        34636833 => 'I',
        211034376 => 'J',
        307334313 => 'K',
        504398881 => 'L',
        311731689 => 'M',
        311735657 => 'N',
        211068198 => 'O',
        34841895 => 'P',
        483697958 => 'Q',
        307338535 => 'R',
        210897190 => 'S',
        69273679 => 'T',
        211068201 => 'U',
        106210601 => 'V',
        318022953 => 'W',
        311630121 => 'X',
        35793193 => 'Y',
        504434959 => 'Z',
        _ => '?',
    }
}

pub fn read_display(display: Vec<Vec<bool>>) -> String {
    crate::prelude::transpose(
        display
            .into_iter()
            .map(|row| {
                row.chunks(5)
                    .into_iter()
                    .map(|rowchunk| {
                        rowchunk
                            .iter()
                            .enumerate()
                            .map(|(pos, pix)| if *pix { 2_usize.pow(pos as u32) } else { 0 })
                            .sum::<usize>()
                    })
                    .collect()
            })
            .collect(),
    )
    .into_iter()
    .map(|digit| {
        digit
            .into_iter()
            .enumerate()
            .map(|(pos, sub)| sub * 32_usize.pow(pos as u32))
            .sum::<usize>()
    })
    .map(lookup_char)
    .collect()
}

/// 2D cartesian coordinate, origin at top-left.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Coord {
    pub x: i32,
    pub y: i32,
}

impl Coord {
    pub const ORIGIN: Coord = Coord::new(0, 0);

    pub const fn new(x: i32, y: i32) -> Coord {
        Coord { x, y }
    }

    pub fn x(self) -> i32 {
        self.x
    }

    pub fn x_as_usize(self) -> usize {
        self.x
            .try_into()
            .unwrap_or_else(|_| panic!("Error converting {:?} to unsigned", self))
    }

    pub fn y(self) -> i32 {
        self.y
    }

    pub fn y_as_usize(self) -> usize {
        self.y
            .try_into()
            .unwrap_or_else(|_| panic!("Error converting {:?} to unsigned", self))
    }

    pub fn vline(x: i32, start: i32, end: i32) -> impl Iterator<Item = Coord> {
        (start..=end).map(move |y| Coord { x, y })
    }

    pub fn hline(y: i32, start: i32, end: i32) -> impl Iterator<Item = Coord> {
        (start..=end).map(move |x| Coord { x, y })
    }

    pub fn span(self, rhs: &Coord) -> impl Iterator<Item = Coord> {
        (self.x.min(rhs.x)..=self.x.max(rhs.x))
            .cartesian_product(self.y.min(rhs.y)..=self.y.max(rhs.y))
            .map(move |(x, y)| Coord { x, y })
    }

    fn translate(self, delta_x: i32, delta_y: i32) -> Coord {
        Coord {
            x: self.x + delta_x,
            y: self.y + delta_y,
        }
    }

    pub fn up(self) -> Coord {
        self.translate(0, -1)
    }

    pub fn down(self) -> Coord {
        self.translate(0, 1)
    }

    pub fn left(self) -> Coord {
        self.translate(-1, 0)
    }

    pub fn right(self) -> Coord {
        self.translate(1, 0)
    }

    pub fn down_left(self) -> Coord {
        self.translate(-1, 1)
    }

    pub fn down_right(self) -> Coord {
        self.translate(1, 1)
    }

    pub fn up_left(self) -> Coord {
        self.translate(1, -1)
    }

    pub fn up_right(self) -> Coord {
        self.translate(-1, -1)
    }

    pub fn chebyshev(&self, tar: Coord) -> i32 {
        Chebyshev.distance(*self, tar)
    }

    pub fn manhattan(&self, tar: Coord) -> i32 {
        Manhattan.distance(*self, tar)
    }
}

impl Add for Coord {
    type Output = Coord;
    fn add(self, rhs: Coord) -> Coord {
        Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Add<&Coord> for Coord {
    type Output = Coord;
    fn add(self, rhs: &Coord) -> Coord {
        Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl std::ops::AddAssign for Coord {
    fn add_assign(&mut self, rhs: Self) {
        *self = Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Sub<Coord> for Coord {
    type Output = Coord;
    fn sub(self, rhs: Coord) -> Coord {
        Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Sub<&Coord> for Coord {
    type Output = Coord;
    fn sub(self, rhs: &Coord) -> Coord {
        Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl std::ops::SubAssign for Coord {
    fn sub_assign(&mut self, rhs: Self) {
        *self = Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Coord3 {
    x: i32,
    y: i32,
    z: i32,
}

impl Coord3 {
    pub const ORIGIN: Coord3 = Coord3::new(0, 0, 0);

    pub const fn new(x: i32, y: i32, z: i32) -> Coord3 {
        Coord3 { x, y, z }
    }

    pub fn x(self) -> i32 {
        self.x
    }

    pub fn x_as_usize(self) -> usize {
        self.x
            .try_into()
            .unwrap_or_else(|_| panic!("Error converting {:?} to unsigned", self))
    }

    pub fn y(self) -> i32 {
        self.y
    }

    pub fn y_as_usize(self) -> usize {
        self.y
            .try_into()
            .unwrap_or_else(|_| panic!("Error converting {:?} to unsigned", self))
    }

    pub fn z(self) -> i32 {
        self.z
    }

    pub fn z_as_usize(self) -> usize {
        self.z
            .try_into()
            .unwrap_or_else(|_| panic!("Error converting {:?} to unsigned", self))
    }

    pub fn from(line: String) -> Coord3 {
        let xyz = line
            .split(",")
            .map(|n| {
                n.parse()
                    .unwrap_or_else(|_| panic!("Error parsing coordinate {}!", n))
            })
            .collect::<Vec<_>>();
        Coord3 {
            x: xyz[0],
            y: xyz[1],
            z: xyz[2],
        }
    }
}

/// L_1 metric (as a taxicab).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Manhattan;

/// L_??? metric (as the king moves in chess).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Chebyshev;

pub trait DiscreteMetric: Copy + Clone + PartialEq {
    fn distance(self, src: Coord, tar: Coord) -> i32;
}

impl DiscreteMetric for Manhattan {
    fn distance(self, src: Coord, tar: Coord) -> i32 {
        let d = src - tar;
        d.x.abs() + d.y.abs()
    }
}

impl DiscreteMetric for Chebyshev {
    fn distance(self, src: Coord, tar: Coord) -> i32 {
        let d = src - tar;
        d.x.abs().max(d.y.abs())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Circle<M> {
    centre: Coord,
    radius: i32,
    metric: M,
}

impl<M: DiscreteMetric> Circle<M> {
    pub fn new(centre: Coord, radius: i32, metric: M) -> Circle<M> {
        Circle {
            centre,
            radius,
            metric,
        }
    }

    pub fn radius(&self) -> i32 {
        self.radius
    }

    pub fn contains(&self, point: &Coord) -> bool {
        self.metric.distance(self.centre, point.to_owned()) <= self.radius
    }

    pub fn grow(&self) -> Circle<M> {
        Circle {
            radius: self.radius + 1,
            ..self.clone()
        }
    }

    pub fn westmost(&self) -> Coord {
        self.centre.translate(-self.radius, 0)
    }

    pub fn eastmost(&self) -> Coord {
        self.centre.translate(self.radius, 0)
    }

    pub fn northmost(&self) -> Coord {
        self.centre.translate(0, -self.radius)
    }

    pub fn southmost(&self) -> Coord {
        self.centre.translate(0, self.radius)
    }

    pub fn covers_x(&self, x: i32) -> bool {
        (self.centre.x - x).abs() <= self.radius
    }

    pub fn covers_y(&self, y: i32) -> bool {
        (self.centre.y - y).abs() <= self.radius
    }
}

impl Circle<Chebyshev> {
    pub fn chebyshev(centre: Coord, radius: i32) -> Circle<Chebyshev> {
        Circle::new(centre, radius, Chebyshev)
    }
}

impl Circle<Manhattan> {
    pub fn manhattan(centre: Coord, radius: i32) -> Circle<Manhattan> {
        Circle::new(centre, radius, Manhattan)
    }

    pub fn boundary(&self) -> Vec<Coord> {
        (0..self.radius)
            .map(|s| {
                [
                    self.centre.translate(s, self.radius - s),
                    self.centre.translate(-s, self.radius - s),
                    self.centre.translate(s, s - self.radius),
                    self.centre.translate(-s, s - self.radius),
                ]
            })
            .collect::<Vec<_>>()
            .concat()
    }
}

impl<M: DiscreteMetric> PartialOrd for Circle<M> {
    fn partial_cmp(&self, rhs: &Circle<M>) -> Option<Ordering> {
        if self.centre == rhs.centre && self.radius == rhs.radius {
            Some(Equal)
        } else if self.clone().contains(&rhs.westmost())
            && self.clone().contains(&rhs.eastmost())
            && self.clone().contains(&rhs.northmost())
            && self.clone().contains(&rhs.southmost())
        {
            Some(Greater)
        } else {
            None
        }
    }
}

pub trait Ascii2d {
    fn to_char(i: Option<&Self>) -> char;
}

pub trait PrintAscii2d {
    fn print_map(&self, top: i32, bottom: i32, left: i32, right: i32);
}

impl<I: Ascii2d> PrintAscii2d for HashMap<Coord, I> {
    /// Printing enabled using a feature, otherwise treat as no-op.

    #[cfg(not(feature = "print"))]
    fn print_map(&self, _: i32, _: i32, _: i32, _: i32) {}

    #[cfg(feature = "print")]
    fn print_map(&self, top: i32, bottom: i32, left: i32, right: i32) {
        {
            (top..=bottom).for_each(|y| {
                (left..=right)
                    .for_each(|x| print!("{}", Ascii2d::to_char(self.get(&Coord::new(x, y)))));
                println!();
            });
            println!();
        }
    }
}

#[macro_export]
macro_rules! map2d {
    ( @closure $p:pat => $tup:expr , $first:expr, $second:expr) => {
        izip!($first, $second).map(|(a, b)| izip!(a, b).map(|$p| $tup))
    };

    ( @closure $p:pat => $tup:expr , $first:expr, $second:expr, $third:expr) => {
        izip!($first, $second, $third).map(|(a, b, c)| izip!(a, b, c).map(|$p| $tup))
    };

    ( @closure $p:pat => $tup:expr , $first:expr, $second:expr, $third:expr, $fourth:expr ) => {
        izip!($first, $second, $third, $fourth).map(|(a, b, c, d)| izip!(a, b, c, d).map(|$p| $tup))
    };
}
