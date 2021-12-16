use std::fs::File;
use std::io::Lines;
use std::io::BufReader;
use std::io::{self, BufRead};
use std::path::Path;
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{space1, digit1},
    error::{context, VerboseError},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
enum Direction {
    Forward,
    Down,
    Up
}

impl From<&str> for Direction {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "forward" => Direction::Forward,
            "up" => Direction::Up,
            "down" => Direction::Down,
            _ => unimplemented!("invalid direction"),
        }
    }
}

struct Day2Step {
    direction: Direction,
    magnitude: String,
}

type Res<T, U> = IResult<T, U, VerboseError<T>>;
fn direction_p(input: &str) -> Res<&str, Direction> {
    context(
        "direction",
        alt((tag_no_case("forward"), tag_no_case("up"), tag_no_case("down"))),
    )(input)
        .map(|(next_input, res)| (next_input, res.into()))
}

fn day2step_p(input: &str) -> Res<&str, Day2Step> {
    context(
        "day2step",
        tuple((direction_p, space1, digit1)),
    )(input)
        .map(|(next_input, res)| (next_input, Day2Step { direction: res.0, magnitude: res.2.to_string() }))

}

fn main() {
    //let day1_1_ans = day1_1();
    let day1_1_ans = match read_lines("./input/Day01.input") {
        Ok(lines) => day1_1(day1convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 1_1: {}", day1_1_ans);

    let day1_2_ans = match read_lines("./input/Day01.input") { 
        Ok(lines) => day1_2(day1convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 1_2: {}", day1_2_ans);

    match direction_p("forward 5") {
        Ok(d) => println!("Got direction result: {:?}", d.1),
        Err(e) => println!("Got error: {}", e),
    }

    match day2step_p("forward 5") {
        Ok(d) => println!("Got step result: {:?}, {}", d.1.direction, d.1.magnitude),
        Err(e) => println!("Got error: {}", e),
    }
    /*
    let day2_1_ans = match read_lines("./input/Day02.input") { 
        Ok(lines) => day2_1(day2convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 1_2: {}", day1_2_ans);
    */
}

fn day1_1(v: Vec<i32>) -> i32 {
    let result = v.iter().fold((0, 0), |acc, unwrapped| {
        if *unwrapped > acc.0 {
            (*unwrapped, acc.1 + 1)
        } else {
            (*unwrapped, acc.1)
        }
    });

    return result.1 - 1;
}

fn day1_2(v: Vec<i32>) -> i32 {
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

fn day2_1<T>(v: T) -> i32 {
    0
}

fn day1convert(lines: Lines<BufReader<File>>) -> Vec<i32> {
    let mut result = Vec::new();
    for l in lines {
        if let Ok(measurement) = l {
            result.push(measurement.parse().unwrap());
        }
    }

    return result
}

fn day2convert(lines: Lines<BufReader<File>>) -> Vec<i32> {
    let mut result = Vec::new();
    for l in lines {
        if let Ok(measurement) = l {
            result.push(measurement.parse().unwrap());
        }
    }

    return result
}

fn read_lines<P>(filename:P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
