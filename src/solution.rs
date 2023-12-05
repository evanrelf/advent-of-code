use once_cell::sync::Lazy;
use std::{fmt::Display, sync::Mutex};

#[linkme::distributed_slice]
pub static SOLUTIONS: [Lazy<Solution>];

pub struct Solution {
    pub year: u16,
    pub day: u8,
    pub part: u8,
    #[allow(clippy::type_complexity)]
    pub solve: Mutex<Option<Box<dyn FnOnce(&str) -> anyhow::Result<String> + Send + Sync>>>,
}

impl Solution {
    pub fn new<F, R>(year: u16, day: u8, part: u8, solve: F) -> Self
    where
        F: FnOnce(&str) -> anyhow::Result<R> + Send + Sync + 'static,
        R: Display,
    {
        let solve = Mutex::new(Some(Box::new(|input: &str| {
            solve(input).map(|solution| solution.to_string())
        }) as _));

        Self {
            year,
            day,
            part,
            solve,
        }
    }
}

macro_rules! aoc {
    ($year:literal, $day:literal, $part:literal, $solve:ident) => {
        #[linkme::distributed_slice($crate::solution::SOLUTIONS)]
        static _SOLUTION: ::once_cell::sync::Lazy<$crate::solution::Solution> =
            ::once_cell::sync::Lazy::new(|| {
                $crate::solution::Solution::new($year, $day, $part, $solve)
            });
    };
}

pub(crate) use aoc;
