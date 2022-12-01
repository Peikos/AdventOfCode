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
    R: Debug + PartialEq + Eq,
{
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
