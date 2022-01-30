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
