use crate::pest::Parser;
use crate::prelude::{run, PuzzleInput};
use itertools::Itertools;
use std::rc::Rc;
use std::sync::Mutex;

#[derive(Parser)]
#[grammar = "day11.pest"]
struct MonkeyParser;

#[derive(Clone, Debug)]
pub struct Monkey {
    business: u64,
    items: Vec<u64>,
    operation: Operation,
    test: Test,
}

impl Monkey {
    pub fn from(mut res: pest::iterators::Pairs<Rule>) -> Monkey {
        res.next();
        Monkey {
            business: 0,
            items: res
                .next()
                .unwrap()
                .into_inner()
                .into_iter()
                .map(|item| item.as_str().parse().expect("Invalid item!"))
                .collect(),
            operation: Operation::from(&mut res),
            test: Test::from(&mut res),
        }
    }

    fn commit(&mut self) {
        self.business += self.items.len() as u64; // add 1 for each item at start of turn
        self.items = Vec::new(); // empty inventory
                                 //dbg!(&self.num, &self.business);
    }

    fn catch(&mut self, item: u64) {
        self.items.push(item);
    }
}

#[derive(Clone, Debug)]
pub enum Operation {
    Add(u64),
    Multiply(u64),
    Square,
}

impl Operation {
    pub fn from(res: &mut pest::iterators::Pairs<Rule>) -> Operation {
        let mut formula = res.next().unwrap().into_inner();
        let a = formula.next().unwrap().as_str();
        let op = formula.next().unwrap().as_str();
        let b = formula.next().unwrap().as_str();
        if a == b && op == "*" {
            Operation::Square
        } else if a == "old" && b.chars().all(|c| c.is_numeric()) {
            match op {
                "*" => Operation::Multiply(b.parse().unwrap()),
                "+" => Operation::Add(b.parse().unwrap()),
                _ => panic!(),
            }
        } else {
            panic!("{},{},{}", a, op, b)
        }
    }

    fn eval(&self, worry: &u64) -> u64 {
        match self {
            Operation::Square => worry * worry,
            Operation::Add(delta) => worry + delta,
            Operation::Multiply(delta) => worry * delta,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Test {
    div_by: u64,
    on_true: usize,
    on_false: usize,
}

impl Test {
    pub fn from(res: &mut pest::iterators::Pairs<Rule>) -> Test {
        Test {
            div_by: res.next().unwrap().as_str().parse().unwrap(),
            on_true: res.next().unwrap().as_str().parse().unwrap(),
            on_false: res.next().unwrap().as_str().parse().unwrap(),
        }
    }
    fn check(&self, worry: &u64) -> usize {
        if worry % self.div_by == 0 {
            self.on_true
        } else {
            self.on_false
        }
    }
}

type Barrel = Vec<Monkey>;

pub fn read_data(lines: PuzzleInput) -> Barrel {
    lines
        .split(|l| l.is_empty())
        .map(|block| {
            let input = block.join("\n");
            let res = MonkeyParser::parse(Rule::monkey, &input)
                .expect("No parse for monkey!")
                .next()
                .expect("Empty result!")
                .into_inner();

            Monkey::from(res)
        })
        .collect::<Vec<Monkey>>()
}

pub fn monkey_business<F>(monkeys: &Barrel, timespan: usize, worry_management: F) -> u64
where
    F: Fn(u64) -> u64,
{
    let mut_monkeys = monkeys
        .iter()
        .cloned()
        .map(|m| Rc::new(Mutex::new(m)))
        .collect::<Vec<_>>();

    (0..timespan).for_each(|_| {
        (0..monkeys.len()).for_each(|m| {
            let mut monkey = mut_monkeys[m].lock().unwrap();
            monkey.items.iter().for_each(|i| {
                let worry = worry_management(monkey.operation.eval(i));
                let target = monkey.test.check(&worry);
                mut_monkeys[target].lock().unwrap().catch(worry);
                //
            });
            monkey.commit();
        });
    });

    mut_monkeys
        .iter()
        .map(|m| m.lock().unwrap().business)
        .sorted()
        .rev()
        .take(2)
        .product::<u64>()
}

pub fn part1(monkeys: &Barrel) -> u64 {
    monkey_business(monkeys, 20, |x| x / 3)
}

pub fn part2(monkeys: &Barrel) -> u64 {
    let modulus = monkeys.iter().map(|m| m.test.div_by).product::<u64>();
    //dbg!(modulus);

    monkey_business(monkeys, 10000, |x| x % modulus)
}

pub fn main() {
    let _ = run(
        11,
        read_data,
        part1,
        part2,
        [
            Some(10605),
            Some(120756),
            Some(2713310158),
            Some(39109444654),
        ],
    );
}
