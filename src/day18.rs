use crate::prelude::{run, Coord3, PuzzleInput};
use ndarray::{Array3, Axis};
use std::collections::BTreeSet;

type Intermediate = Array3<bool>;

/// Convolution with an edge-detecting kernel to count exposed cube faces.
pub fn count(array: &Array3<bool>) -> usize {
    array
        .windows((3, 3, 3))
        .into_iter()
        .map(|cube| {
            if cube[(1, 1, 1)] {
                6 - [
                    (0, 1, 1),
                    (2, 1, 1),
                    (1, 0, 1),
                    (1, 2, 1),
                    (1, 1, 0),
                    (1, 1, 2),
                ]
                .into_iter()
                .filter(|neighbour| cube[*neighbour])
                .count()
            } else {
                0
            }
        })
        .sum()
}

pub fn read_data(lines: PuzzleInput) -> Intermediate {
    let lava = lines.into_iter().map(Coord3::from).collect::<Vec<_>>();

    let size = if lava.len() < 15 { 7 } else { 20 };

    let mut array: Array3<bool> = Array3::from_elem((2 + size, 2 + size, 2 + size), false); // Two larger than needed
    lava.iter().for_each(|cell| {
        array[(
            1 + cell.x_as_usize(), // Offset to include border cells in count
            1 + cell.y_as_usize(),
            1 + cell.z_as_usize(),
        )] = true;
    });

    array
}

pub fn part1(lava: &Intermediate) -> usize {
    count(&lava)
}

pub fn part2(lava: &Intermediate) -> usize {
    let size = *lava
        .shape()
        .iter()
        .max()
        .expect("Cannot get size from lava-array!")
        - 2; // Compensate for larger array

    let mut fill_queue = BTreeSet::new();
    let mut filled = BTreeSet::new();
    let mut new: Array3<bool> = Array3::from_elem(lava.dim(), true);

    fill_queue.insert((1, 1, 1));

    while let Some(voxel) = fill_queue.pop_first() {
        new[voxel] = false;

        let (x, y, z) = voxel;
        let l = (x.saturating_sub(1), y, z);
        let r = (x + 1, y, z);
        let d = (x, y.saturating_sub(1), z);
        let u = (x, y + 1, z);
        let f = (x, y, z.saturating_sub(1));
        let b = (x, y, z + 1);

        if x > 0 && !lava[l] && !filled.contains(&l) {
            fill_queue.insert(l);
        } else if x <= size && !lava[r] && !filled.contains(&r) {
            fill_queue.insert(r);
        }
        if y > 0 && !lava[d] && !filled.contains(&d) {
            fill_queue.insert(d);
        } else if y <= size && !lava[u] && !filled.contains(&u) {
            fill_queue.insert(u);
        }
        if z > 0 && !lava[f] && !filled.contains(&f) {
            fill_queue.insert(f);
        } else if z <= size && !lava[b] && !filled.contains(&b) {
            fill_queue.insert(b);
        }

        filled.insert(voxel);
    }

    #[cfg(feature = "print")]
    {
        print(&lava);
        print(&new);
    }

    count(&new)
}

pub fn main() {
    let _ = run(
        18,
        read_data,
        part1,
        part2,
        [Some(64), Some(3448), Some(58), Some(2052)],
    );
}

pub fn print(array: &Array3<bool>) {
    (0_usize..array.len_of(Axis(2))).into_iter().for_each(|z| {
        (0_usize..array.len_of(Axis(1))).into_iter().for_each(|y| {
            (0_usize..array.len_of(Axis(0))).into_iter().for_each(|x| {
                print!("{}", if array[(x, y, z)] { '#' } else { ' ' });
            });
            println!();
        });
        println!();
        println!();
    });
}
