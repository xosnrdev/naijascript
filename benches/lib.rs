use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::{Lexer, Token};

fn bench_lexer(c: &mut Criterion) {
    let input = black_box(
        r#"make x get 5\n\
         shout (x add 1)\n\
         if to say (x na 5) start make y get 1 end\n\
         jasi (x small pass 10) start make x get x add 1 end\n"#,
    );
    c.bench_function("lexer", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            loop {
                let st = lexer.next_token();
                if st.token == Token::EOF {
                    break;
                }
                black_box(st);
            }
        })
    });
}

fn bench_parser(c: &mut Criterion) {
    let input = black_box(
        r#"make x get 5\n\
         shout (x add 1)\n\
         if to say (x na 5) start make y get 1 end\n\
         jasi (x small pass 10) start make x get x add 1 end\n"#,
    );
    c.bench_function("parser", |b| {
        b.iter(|| {
            let mut parser = Parser::new(input);
            black_box(parser.parse_program());
        })
    });
}

criterion_group!(benches, bench_lexer, bench_parser,);
criterion_main!(benches);
