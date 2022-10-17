mod tokenizer;
mod parser;
fn main() {
    let input = r#"2 * 3 + 5"#;
    let inputstream = input.as_bytes().to_vec().into_iter().map(|x| Ok(x));
    let tokenizer = tokenizer::Tokenizer::new(inputstream);
    let parser = parser::Parser::new(tokenizer.map(|tok| tok.unwrap()));

    for node in parser {
        println!("{:#?}", node);
    }
}