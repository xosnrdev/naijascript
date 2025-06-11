use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::syntax::Lexer;

fn bench_lexer_tokenize_small(c: &mut Criterion) {
    let input = black_box("make x get 5\nx add 3 times 2\nif to say (x na 5)\nsmall pass\n");
    c.bench_function("lexer_tokenize_small", |b| {
        b.iter(|| {
            let lexer = Lexer::new(input);
            let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
            black_box(tokens);
        })
    });
}

fn bench_lexer_tokenize_large(c: &mut Criterion) {
    let input = black_box(
        (0..1000)
            .map(|_| "make x get 5\nx add 3 times 2\nif to say (x na 5)\nsmall pass\n")
            .collect::<String>(),
    );
    c.bench_function("lexer_tokenize_large", |b| {
        b.iter(|| {
            let lexer = Lexer::new(&input);
            let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
            black_box(tokens);
        })
    });
}

fn bench_lexer_identifier_hotpath(c: &mut Criterion) {
    let input = black_box("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_");
    c.bench_function("lexer_identifier_hotpath", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            let _ = lexer.next();
        })
    });
}

fn bench_lexer_number_hotpath(c: &mut Criterion) {
    let input = black_box("1234567890.1234567890");
    c.bench_function("lexer_number_hotpath", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            let _ = lexer.next();
        })
    });
}

fn bench_lexer_multiword_keyword_hotpath(c: &mut Criterion) {
    let input = black_box("if to say small pass if not so");
    c.bench_function("lexer_multiword_keyword_hotpath", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(input);
            let _ = lexer.next();
            let _ = lexer.next();
            let _ = lexer.next();
        })
    });
}

criterion_group!(
    benches,
    bench_lexer_tokenize_small,
    bench_lexer_tokenize_large,
    bench_lexer_identifier_hotpath,
    bench_lexer_number_hotpath,
    bench_lexer_multiword_keyword_hotpath
);
criterion_main!(benches);
