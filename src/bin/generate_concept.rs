extern crate fca;

use std::env;
use std::io::Read;
use std::fs::File;
use fca::formalconcept::*;

fn main() {
    match env::args().nth(1) {
        Some(t) => {
            let mut f = File::open(t).unwrap();
            println!("Loading data..");
            let mut buffer = String::new();
            let _ = f.read_to_string(&mut buffer);
            let mut fc = ConceptLattice::<String, String>::new();
            println!("Reading data..");
            fc.read_csv(&buffer, b',');
            println!("Enumerating..");
            fc.enumerate_seq().enumerate();
            //for item in fc.concepts { println!("{:?}", item); }
            println!("{} concepts generated.", fc.concepts.len());
            match env::args().nth(2) {
                Some(ref t) if t == "-v" => {
                    for fc in fc.concepts { println!("{}", fc); }
                },
                _ => {},
            }
        },
        _ => {},
    }
}
