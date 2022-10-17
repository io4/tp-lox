mod tokenizer;
mod parser;

use std::io::{BufReader, Read, stdin};

fn main() {
    let input = BufReader::new(stdin());
    let inputstream = input.bytes();
    let tokenizer = tokenizer::Tokenizer::new(inputstream);
    let parser = parser::Parser::new(tokenizer.map(|tok| tok.unwrap()));

    for node in parser {
        println!("{:#?}", node);
    }
}