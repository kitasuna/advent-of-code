use itertools::Itertools;
use std::fs::File;
use std::io::Lines;
use std::io::BufReader;

pub fn part1(v: Vec<i32>) -> i32 {
    let result = v.iter().fold((0, 0), |acc, unwrapped| {
        if *unwrapped > acc.0 {
            (*unwrapped, acc.1 + 1)
        } else {
            (*unwrapped, acc.1)
        }
    });

    return result.1 - 1;
}

pub fn part2(v: Vec<i32>) -> i32 {
    let mut increases = 0;
    let mut last_window = 0;
    for (a, b, c) in v.iter().tuple_windows::<(_, _, _)>() {
        let this_window = *a + *b + *c;
        if this_window > last_window {
            increases += 1
        }
        last_window = this_window;
    }
    return increases - 1;
}

pub fn convert(lines: Lines<BufReader<File>>) -> Vec<i32> {
    let mut result = Vec::new();
    for l in lines {
        if let Ok(measurement) = l {
            result.push(measurement.parse().unwrap());
        }
    }

    return result
}

