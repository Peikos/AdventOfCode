use crate::prelude::{cons, transpose, ExtraIterators};
use crate::prelude::{run, PuzzleInput};
use itertools::{izip, Itertools};
use ndarray::Array2;

type Forest = Array2<i32>;

pub fn read_data(lines: PuzzleInput) -> Forest {
    let mut rows = 0;

    let m: Vec<i32> = lines
        .into_iter()
        .map(|line| {
            rows += 1;
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect()
        })
        .concat();

    let cols = m.len() / rows;

    Forest::from_shape_vec((rows, cols), m).unwrap()
}

/// Swipe left to right, top to bottom, etc. to determine intermediate score from each cardinal direction. Requires parameters for map_accum and a way to combine the four sub-results.
fn scan_directions<TreeScore, InitAccum, MapAccumF, QuaterFoldMatrix>(
    forest: &Forest,
    init: InitAccum,
    map_acum_func: MapAccumF,
    fold_four_matrices: QuaterFoldMatrix,
) -> usize
where
    InitAccum: Clone,
    MapAccumF: Copy + FnMut(&InitAccum, &i32) -> Option<(InitAccum, TreeScore)>,
    QuaterFoldMatrix: Fn([Vec<Vec<TreeScore>>; 4]) -> usize,
{
    let left_to_right = forest
        .rows()
        .into_iter()
        .map(|row| row.iter().map_accum(init.clone(), map_acum_func).collect())
        .collect();

    let top_down = forest
        .columns()
        .into_iter()
        .map(|row| row.iter().map_accum(init.clone(), map_acum_func).collect())
        .collect();

    let right_to_left = forest
        .rows()
        .into_iter()
        .map(|col| {
            col.iter()
                .rev()
                .map_accum(init.clone(), map_acum_func)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect()
        })
        .collect();

    let bottom_up = forest
        .columns()
        .into_iter()
        .map(|col| {
            col.iter()
                .rev()
                .map_accum(init.clone(), map_acum_func)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect()
        })
        .collect();

    // Map quartary operator over the 2D grid to tally up and compute score.

    fold_four_matrices([
        left_to_right,
        transpose(top_down),
        right_to_left,
        transpose(bottom_up),
    ])
}

pub fn part1(forest: &Forest) -> usize {
    scan_directions(
        forest,
        -1,
        // Function for map_accum: keep track of highest tree so far, return true whenever a new
        // maximum is encountered, false otherwise.
        |state: &i32, current: &i32| {
            if current > state {
                Some((*current, true))
            } else {
                Some((*state, false))
            }
        },
        // Map quartary `or` over the 2D grid, as being visible from any direction suffices.
        |[left_to_right, top_down, right_to_left, bottom_up]: [Vec<Vec<bool>>; 4]| {
            crate::map2d!(
            @closure (a, b, c, d) => (a || b || c || d),
                left_to_right,
                top_down,
                right_to_left,
                bottom_up
                )
            .map(|r| r.filter(|x| *x).count())
            .sum()
        },
    )
}
pub fn part2(forest: &Forest) -> usize {
    scan_directions(
        forest,
        Vec::new(),
        // Function for map_accum: keep history to cast a ray and determine nearest tree to block
        // line of sight.
        |state: &Vec<usize>, current: &i32| {
            let height = *current as usize;
            let mut trees = state.into_iter().rev();

            // Cast a ray until a tree is hit.
            let mut score: usize = trees.peeking_take_while(|t| *t < &height).count();

            // If the iterator is not exhausted, count the blocking tree as well.
            if trees.next().is_some() {
                score += 1
            };

            Some((cons(state.to_vec(), height), score))
        },
        // Map quartary product over the 2D grid, per problem description.
        |[left_to_right, top_down, right_to_left, bottom_up]: [Vec<Vec<usize>>; 4]| {
            crate::map2d!(
            @closure (a, b, c, d) => (a * b * c * d),
                    left_to_right,
                    top_down,
                    right_to_left,
                    bottom_up
                    )
            .map(|r| r.max().unwrap())
            .max()
            .unwrap()
        },
    )
}

pub fn main() {
    let _ = run(
        8,
        read_data,
        part1,
        part2,
        [Some(21), Some(1703), Some(8), Some(496650)],
    );
}
