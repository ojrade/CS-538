#![allow(dead_code)]
#![forbid(unsafe_code)]

mod exercise1;
mod exercise2;
mod exercise3;
mod exercise4;

mod parser;
mod rpn;

fn main() {
    if let Err(err) = parser::rpn_repl() {
        println!("Error: {:?}", err);
    }
}
