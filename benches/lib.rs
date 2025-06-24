use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;

fn bench_lexer(c: &mut Criterion) {
    let input = black_box(
        "make x get 5\n\
         shout (x add 1)\n\
         if to say (x na 5) start make y get 1 end\n\
         jasi (x small pass 10) start make x get x add 1 end\n",
    );
    c.bench_function("lexer", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            let tokens = lexer.next_token();
            black_box(tokens);
        })
    });
}

fn bench_parser(c: &mut Criterion) {
    let input = black_box(
        "make x get 5\n\
         shout (x add 1)\n\
         if to say (x na 5) start make y get 1 end\n\
         jasi (x small pass 10) start make x get x add 1 end\n",
    );
    c.bench_function("parser", |b| {
        b.iter(|| {
            let mut parser = Parser::new(input);
            let (bid, _) = parser.parse_program();
            black_box(bid);
        })
    });
}

criterion_group!(benches, bench_lexer, bench_parser,);
criterion_main!(benches);
