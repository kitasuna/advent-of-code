use std::fs::File;
use std::io::Lines;
use std::io::BufReader;
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{space1, digit1},
    error::{context, VerboseError},
    sequence::tuple,
    IResult,
};

#[derive(Debug, PartialEq)]
enum Direction {
    Forward,
    Down,
    Up
}

#[derive(Debug)]
pub struct Day2Step {
    direction: Direction,
    magnitude: i32,
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

type Res<T, U> = IResult<T, U, VerboseError<T>>;
fn direction_p(input: &str) -> Res<&str, Direction> {
    context(
        "direction",
        alt((tag_no_case("forward"), tag_no_case("up"), tag_no_case("down"))),
    )(input)
        .map(|(next_input, res)| (next_input, res.into()))
}

fn day2step_p(input: &String) -> Res<&str, Day2Step> {
    context(
        "day2step",
        tuple((direction_p, space1, digit1)),
    )(input)
        .map(|(next_input, res)| (next_input, Day2Step { direction: res.0, magnitude: res.2.to_string().parse().unwrap() }))

}
pub fn part1(v: Vec<Day2Step>) -> i32 {
   // (Horiz, Depth)
   let result = v.iter().fold((0, 0), |mut acc, step| {
        if step.direction == Direction::Forward {
            acc.0 += step.magnitude;
        } else if step.direction == Direction::Up {
            acc.1 -= step.magnitude;
        } else if step.direction == Direction::Down {
            acc.1 += step.magnitude;
        }
        return acc
   });

   return result.0 * result.1
}

pub fn part2(v: Vec<Day2Step>) -> i32 {
   // (Horiz, Depth, Aim)
   let result = v.iter().fold((0, 0, 0), |mut acc, step| {
        if step.direction == Direction::Forward {
            acc.0 += step.magnitude;
            acc.1 += acc.2 * step.magnitude
        } else if step.direction == Direction::Up {
            acc.2 -= step.magnitude;
        } else if step.direction == Direction::Down {
            acc.2 += step.magnitude
        }
        return acc
   });

   return result.0 * result.1
}

pub fn convert(lines: Lines<BufReader<File>>) -> Vec<Day2Step> {
    let mut result = Vec::new();
    for l in lines {
        match l {
            Ok(s) => {
                let parsed_res = day2step_p(&s);
                if let Ok(parsed) = parsed_res {
                   result.push(parsed.1) 
                }
            },
            Err(e) => panic!("couldn't parse line: {}", e),
        }
    }

    return result
}
