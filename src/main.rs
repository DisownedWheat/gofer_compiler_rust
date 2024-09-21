mod lexer;
mod parser;
fn main() {
    let tokens = lexer::logos_lexer::lex("./test_file").expect("Failed to lex file");
    let filtered_tokens = tokens
        .into_iter()
        .filter(|x| x.is_ok())
        .map(|x| x.unwrap())
        .collect();
    println!("{:?}", filtered_tokens);
    println!("{:?}", parser::parser::parse(filtered_tokens));
}
