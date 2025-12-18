use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::arena::{self, scratch_arena};
use naijascript::builtins;
use naijascript::helpers::MEBI;
use naijascript::syntax::parser::Parser;
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
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let token: Vec<_> = lexer.collect();
            black_box(token);
        });
    });
}

fn bench_scan_string(c: &mut Criterion) {
    let src = r#"
        # String literal
        "abcdefghijklmnopqrstuvwxyz"
        # String escape sequence
        "abcd\nefgh\tijkl\"mnop\\qrst"
    "#;
    c.bench_function("scan_string", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let token: Vec<_> = lexer.collect();
            black_box(token);
        });
    });
}

fn bench_parse_assignment(c: &mut Criterion) {
    let src = "make x get 42";
    c.bench_function("parse_assignment", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_parse_arithmetic_expr(c: &mut Criterion) {
    let src = "make result get 1 add 2 times 3 divide 4 minus 5";
    c.bench_function("parse_arithmetic_expression", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_parse_fn_def(c: &mut Criterion) {
    let src = r"
        do fibonacci(n) start
            if to say (n small pass 2) start
                return n
            end
            return fibonacci(n minus 1) add fibonacci(n minus 2)
        end
    ";

    c.bench_function("parse_function_definition", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_parse_ctrl_flow(c: &mut Criterion) {
    let src = r#"
        make i get 0
        jasi (i small pass 10) start
            if to say (i mod 2 na 0) start
                shout("even")
            end if not so start
                shout("odd")
            end
            i get i add 1
        end
    "#;

    c.bench_function("parse_control_flow", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_parse_string_interpolation(c: &mut Criterion) {
    let src = r#"
        make name get "World"
        make greeting get "Hello {name}, how you dey?"
        shout(greeting)
    "#;

    c.bench_function("parse_string_interpolation", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_parse_nested_expr(c: &mut Criterion) {
    let src = "make result get ((((1 add 2) times 3) divide 4) minus 5) add ((6 times 7) divide (8 add 9))";

    c.bench_function("parse_nested_expression", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
        });
    });
}

fn bench_string_slice(c: &mut Criterion) {
    let src = "Hello, 世界! 🌎";

    c.bench_function("string_slice", |b| {
        b.iter(|| {
            let arena = scratch_arena(None);
            let s = builtins::StringBuiltin::slice(src, 0.0, 5.0, &arena);
            black_box(s);
        });
    });
}

fn bench_two_way(c: &mut Criterion) {
    let src = "ab cd ef gh ij kl mn op qr st uv wx yz";

    c.bench_function("two_way", |b| {
        b.iter(|| {
            let index = builtins::find(src, "uv");
            black_box(index);
        });
    });
}

fn bench(c: &mut Criterion) {
    arena::init(128 * MEBI).unwrap();

    bench_two_way(c);
    bench_string_slice(c);
    bench_skip_comment(c);
    bench_scan_string(c);
    bench_parse_assignment(c);
    bench_parse_arithmetic_expr(c);
    bench_parse_fn_def(c);
    bench_parse_ctrl_flow(c);
    bench_parse_string_interpolation(c);
    bench_parse_nested_expr(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
