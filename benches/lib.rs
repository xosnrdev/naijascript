use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::syntax::scanner::Lexer;

fn bench_skip_comment(c: &mut Criterion) {
    let src = r#"
        # Single line comment
        make x get "foo" # Trailing comment
        # Multi
        # Line
        # Comment
        x add "baz"
    "#;

    c.bench_function("skip_comment", |b| {
        b.iter(|| {
            let lexer = Lexer::new(src);
            let token: Vec<_> = lexer.collect();
            black_box(token);
        })
    });
}

criterion_group!(benches, bench_skip_comment);
criterion_main!(benches);
