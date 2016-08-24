extern crate rand;

use std::env;
use rand::Rng;

fn main() {
    let arg = env::args().collect::<Vec<_>>();
    if arg.len() == 4 {
        print!("header");
        let noo = arg[1].parse::<usize>().unwrap();
        let noa = arg[2].parse::<usize>().unwrap();
        let p = arg[3].parse::<f32>().unwrap();
        for i in 0 .. noa { print!(",attr{}", i); }
        println!("");

        let mut rng = rand::thread_rng();

        for i in 0 .. noo {
            print!("obj{}", i);
            for _ in 0 .. noa { print!(",{}", if rng.gen_range(0f32, 1f32) < p { 1 } else { 0} ); }
            println!("");
        }

    } else {
        println!("Usage: {} number_of_object number_of_attrituse p", arg[0]);
    }
}

