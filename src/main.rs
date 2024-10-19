mod lexer;
mod parser;
fn main() {
    let tokens = lexer::lexer::lex("./test_file").expect("Failed to lex file");
    let filtered_tokens = tokens
        .into_iter()
        .filter(|x| {
            if x.is_ok() {
                return true;
            } else {
                println!("{:?}", x);
                return false;
            }
        })
        .map(|x| x.unwrap())
        .filter(|x| match x {
            lexer::lexer::Token::Whitespace => false,
            _ => true,
        })
        .collect();
    // println!("{:?}", filtered_tokens);
    let _ = parser::parser::parse(filtered_tokens)
        .map(|x| x.print())
        .map_err(|x| println!("{:?}", x));
}
