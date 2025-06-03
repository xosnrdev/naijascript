use std::env;
use std::fs;

use naijascript::syntax::Lexer;
use naijascript::syntax::parser::Parser;

fn main() {
    let mut args = env::args().skip(1);
    let input = if let Some(file) = args.next() {
        fs::read_to_string(file).expect("Failed to read file")
    } else {
        use std::io::Read;
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .expect("Failed to read stdin");
        buf
    };
    let mut parser = Parser::new(Lexer::new(&input));
    match parser.parse_program() {
        Ok(ast) => println!("{ast}"),
        Err(e) => eprintln!("Parse error: {e}"),
    }
}
