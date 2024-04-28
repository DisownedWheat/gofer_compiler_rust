mod lexer;
mod parser;
fn main() {
    let results = lexer::lexer::lex("./test_file".to_string());
    println!("{:?}", results);
    println!("{:?}", parser::parser::parse(&results[..]));
}
