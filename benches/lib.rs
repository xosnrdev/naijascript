use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::{Lexer, Token};

fn bench_lexer(c: &mut Criterion) {
    let input = black_box(
        r#"make x get 42 add 3.14 minus "hello" times (y divide z) shout(result) if to say (a na b) start jasi (c pass d) start end end if not so start make small pass 1 end"#,
    );
    c.bench_function("lexer", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            loop {
                let st = lexer.next().unwrap_or_default();
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
        r#"make x get 5 add 2 x get x times 3 shout(x) if to say (x pass 10) start shout(1) end if not so start jasi (x small pass 20) start x get x add 1 end end"#,
    );
    c.bench_function("parser", |b| {
        b.iter(|| {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            black_box(parser.parse_program());
        })
    });
}

criterion_group!(benches, bench_lexer, bench_parser);
criterion_main!(benches);
