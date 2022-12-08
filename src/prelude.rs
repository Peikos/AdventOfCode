use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::path::PathBuf;
use std::time::Instant;

pub enum Data {
    Test,
    Input,
}

pub type PuzzleInput = Lines<BufReader<File>>;

impl Data {
    pub fn load_input(&self, day: u8) -> Lines<BufReader<File>> {
        let mut path: PathBuf = ["data"].iter().collect();
        path.push(day.to_string());
        path.push(match self {
            Data::Test => "test",
            Data::Input => "input",
        });

        let file = File::open(&path).expect(&format!("File not found: {:?}!", path));
        let buf_reader = BufReader::new(file);
        buf_reader.lines()
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

    println!("┌ DAY {: >2} ┐", day);
    println!("├────────┴─────────────┬──────────────────────┐");
    println!(
        "│ part 1: {: >12?} │ part 2: {: >12?} │",
        duration1 / 1000,
        duration2 / 1000
    );
    println!("└──────────────────────┴──────────────────────┘");
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
    fn map_accum<Acc, F, B>(self, accu: Acc, f: F) -> MapAccum<Self, Acc, F>
    where
        Self: Sized,
        F: FnMut(&Acc, Self::Item) -> Option<(Acc, B)>,
    {
        MapAccum::new(self, accu, f)
    }
}

impl<T: ?Sized> ExtraIterators for T where T: Iterator {}

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
pub fn cons<A>(vec: Vec<A>, item: A) -> Vec<A> {
    let mut vec = vec;
    vec.push(item);
    vec
}
