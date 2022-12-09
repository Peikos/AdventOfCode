use crate::prelude::{run, PuzzleInput};

use recursion::recursive::{Collapse, Expand};
use recursion::recursive_tree::RecursiveTree;
use recursion::{map_layer::MapLayer, recursive_tree::arena_eval::ArenaIndex};

type Tokenised = Vec<Token>;

/// Base functor for tree F-(co)algebras.
#[derive(Debug, Clone)]
pub enum FsLayer<A> {
    Dir { name: String, files: Vec<A> },
    File { name: String, size: u32 },
}

impl<A, B> MapLayer<B> for FsLayer<A> {
    type To = FsLayer<B>;
    type Unwrapped = A;

    fn map_layer<F: FnMut(Self::Unwrapped) -> B>(self, f: F) -> Self::To {
        match self {
            FsLayer::Dir { name, files } => FsLayer::Dir {
                name,
                files: files.into_iter().map(f).collect(),
            },
            FsLayer::File { name, size } => FsLayer::File { name, size },
        }
    }
}

struct FileTree(RecursiveTree<FsLayer<ArenaIndex>, ArenaIndex>);

impl FileTree {
    /// Anamorphism to build a tree from a puzzle input.
    fn build(fs: Tokenised) -> FileTree {
        FileTree(RecursiveTree::expand_layers(
            Token::Dir("/".to_string()),
            |dir_entry| Self::build_layer(dir_entry, fs.clone()),
        ))
    }

    // Build a single layer.
    fn build_layer(entry: Token, ls: Tokenised) -> FsLayer<Token> {
        match entry {
            Token::Dir(name) => {
                let mut input = ls.into_iter();
                input.find(|n| *n == Token::Enter(name.clone()));
                let files: Tokenised = input.take_while(|f| f.file()).collect();
                FsLayer::Dir { name, files }
            }
            Token::File(name, size) => FsLayer::File { name, size },
            s => panic!("{:?}", s),
        }
    }

    /// Catamorphism on file-tree, calculating total file size. Also returns a vec of intermediate
    /// directory size values.
    pub fn dir_sizes(self) -> (u32, Vec<u32>) {
        let mut dirs = Vec::new();

        let total_size = self.0.collapse_layers(|layer| match layer {
            FsLayer::Dir { name: _, files } => {
                let dir_size = files.into_iter().sum();
                dirs.push(dir_size);
                dir_size
            }
            FsLayer::File { name: _, size } => size,
        });

        (total_size, dirs)
    }
}

// Crude tokenisation to facilitate tree building.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Enter(String), // Requests directory contents.
    Dir(String),   // Points towards a directory.
    File(String, u32),
}

impl Token {
    fn file(&self) -> bool {
        matches!(self, Self::File(_, _) | Self::Dir(_))
    }

    /// Quick and dirty tokeniser. Keeps track of paths to deal with duplicate directory names.
    fn try_parse(line: &str, path: &mut Vec<String>) -> Option<Token> {
        if line.contains("$ cd") {
            if line[5..].is_empty() {
                // cd .. is only relevant for path management.
                path.pop();
                None
            } else {
                let dir_name = line[5..].to_string();
                path.push(dir_name);
                Some(Token::Enter(path.join(":")))
            }
        } else if line[0..1] == *"$" {
            // ls commands are not relevant.
            None
        } else if line[0..3] == *"dir" {
            Some(Token::Dir(
                [path.join(":"), ":".to_string(), line[4..].to_string()].concat(),
            ))
        } else {
            let split: Vec<&str> = line.split_whitespace().collect();
            Some(Token::File(
                [path.join(":"), ":".to_string(), split[1].to_string()].concat(),
                split[0].parse().ok()?,
            ))
        }
    }
}

pub fn read_data(lines: PuzzleInput) -> Vec<Token> {
    let mut path = Vec::new();
    lines
        .iter()
        .filter_map(|line| Token::try_parse(line, &mut path))
        .collect()
}

pub fn part1(ls: &Tokenised) -> u32 {
    let fs: FileTree = FileTree::build(ls.to_vec());

    let (_, dir_sizes) = fs.dir_sizes();

    dir_sizes.into_iter().filter(|size| size < &100000).sum()
}

pub fn part2(ls: &Tokenised) -> u32 {
    let fs: FileTree = FileTree::build(ls.to_vec());

    let (total, dir_sizes) = fs.dir_sizes();

    let target = total - 40000000;

    dir_sizes
        .into_iter()
        .filter(|size| size > &target)
        .min()
        .expect("No directories above threshold.")
}

pub fn main() {
    let _ = run(
        7,
        read_data,
        part1,
        part2,
        [Some(95437), Some(1297683), Some(24933642), Some(5756764)],
    );
}
