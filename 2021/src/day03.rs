use std::fs::File;
use std::io::Lines;
use std::io::BufReader;

pub fn convert(lines: Lines<BufReader<File>>) -> Vec<isize> {
    let mut result = Vec::new();
    for l in lines {
        match l {
            Ok(s) => {
                // https://stackoverflow.com/questions/27606616/is-there-anything-in-rust-to-convert-a-binary-string-to-an-integer
                // https://doc.rust-lang.org/std/primitive.isize.html
                let intval = isize::from_str_radix(&s, 2).unwrap();
                result.push(intval);
            },
            Err(e) => panic!("couldn't parse line: {}", e),
        }
    }

    return result
}

pub fn part1(xs: Vec<isize>) -> i32 {
    let mut gamma = 0;
    for i in 0..12 {
        let result = xs.iter().fold((0, 0), |acc, x| {
            if *x & (1 << i) > 0 {
                return (acc.0, acc.1 + 1)
            } else {
                return (acc.0 + 1, acc.1)
            }
        });

        if result.1 > result.0 {
            gamma |= 1 << i;
        }
    }

    return gamma * (gamma ^ 0b111111111111);
}

pub fn part2(xs: Vec<isize>) -> isize {
    let bit_count = 12;
    let mut o2_filtered = xs.clone();
    let mut co2_filtered = xs.clone();
    for bit in 0..bit_count {
        // (ZeroCount, OneCount)
        let mut counts: (i32, i32) = (0,0);
        let mut with_ones = Vec::new();
        let mut with_zeroes = Vec::new();

        for (_i, h) in o2_filtered.iter().enumerate() {
            if (*h & (1 << (bit_count - bit - 1))) > 0 {
                counts.1 += 1;
                with_ones.push(*h);
            } else {
                counts.0 += 1;
                with_zeroes.push(*h);
            }
        }

        if counts.1 >= counts.0 {
            o2_filtered = with_ones;
        } else {
            o2_filtered = with_zeroes;
        }
    }

    for bit in 0..bit_count {
        // (ZeroCount, OneCount)
        let mut counts: (i32, i32) = (0,0);
        let mut with_ones = Vec::new();
        let mut with_zeroes = Vec::new();

        for (_i, h) in co2_filtered.iter().enumerate() {
            if (*h & (1 << (bit_count - bit - 1))) > 0 {
                counts.1 += 1;
                with_ones.push(*h);
            } else {
                counts.0 += 1;
                with_zeroes.push(*h);
            }
        }

        if counts.0 <= counts.1 {
            co2_filtered = with_zeroes;
        } else {
            co2_filtered = with_ones;
        }

        if co2_filtered.len() == 1 {
            break;
        }
    }

    match o2_filtered.first() {
        Some(x) => match co2_filtered.first() {
            Some(y) => x * y,
            None => 0
        },
        None => 0
    }
}
