use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

mod day01;
mod day02;
mod day03;

fn main() {
    let day1_1_ans = match read_lines("./input/Day01.input") {
        Ok(lines) => day01::part1(day01::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 1_1: {}", day1_1_ans);

    let day1_2_ans = match read_lines("./input/Day01.input") { 
        Ok(lines) => day01::part2(day01::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 1_2: {}", day1_2_ans);

    let day2_1_ans = match read_lines("./input/Day02.input") { 
        Ok(lines) => day02::part1(day02::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 2_1: {}", day2_1_ans);

    let day2_2_ans = match read_lines("./input/Day02.input") { 
        Ok(lines) => day02::part2(day02::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 2_2: {}", day2_2_ans);

    let day3_1_ans = match read_lines("./input/Day03.input") {
        Ok(lines) => day03::part1(day03::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 3_1: {:?}", day3_1_ans);

    let day3_2_ans = match read_lines("./input/Day03.input") {
        Ok(lines) => day03::part2(day03::convert(lines)),
        Err(e) => panic!("couldn't open file{}", e),
    };
    println!("Day 3_2: {:?}", day3_2_ans);
}


fn read_lines<P>(filename:P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
