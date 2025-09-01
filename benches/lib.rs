use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::arena::{self, scratch_arena};
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use naijascript::{MEBI, builtins};

fn bench_skip_comment(c: &mut Criterion) {
    let src = r#"
        # Single line comment
        make x get "foo" # Trailing comment
        # Multi
        # Line
        # Comment
        x add "baz"
    "#;
    let arena = scratch_arena(None);
    c.bench_function("skip_comment", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let token: Vec<_> = lexer.collect();
            black_box(token);
            unsafe {
                arena.reset(offset);
            }
        })
    });
}

fn bench_scan_string(c: &mut Criterion) {
    let src = r#"
        # String literal
        "abcdefghijklmnopqrstuvwxyz"
        # String escape sequence
        "abcd\nefgh\tijkl\"mnop\\qrst"
    "#;
    let arena = scratch_arena(None);
    c.bench_function("scan_string", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let token: Vec<_> = lexer.collect();
            black_box(token);
            unsafe {
                arena.reset(offset);
            }
        })
    });
}

fn bench_parse_assignment(c: &mut Criterion) {
    let src = "make x get 42";
    let arena = scratch_arena(None);
    c.bench_function("parse_assignment", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parse_arithmetic_expr(c: &mut Criterion) {
    let src = "make result get 1 add 2 times 3 divide 4 minus 5";
    let arena = scratch_arena(None);
    c.bench_function("parse_arithmetic_expression", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parse_fn_def(c: &mut Criterion) {
    let src = r#"
        do fibonacci(n) start
            if to say (n small pass 2) start
                return n
            end
            return fibonacci(n minus 1) add fibonacci(n minus 2)
        end
    "#;
    let arena = scratch_arena(None);
    c.bench_function("parse_function_definition", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
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
    let arena = scratch_arena(None);
    c.bench_function("parse_control_flow", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parse_long_program(c: &mut Criterion) {
    let src = r#"
        do factorial(n) start
            if to say (n small pass 2) start
                return 1
            end
            return n times factorial(n minus 1)
        end

        do fibonacci(n) start
            if to say (n small pass 2) start
                return n
            end
            return fibonacci(n minus 1) add fibonacci(n minus 2)
        end

        do gcd(a, b) start
            jasi (b no be 0) start
                make temp get b
                b get a mod b
                a get temp
            end
            return a
        end

        make i get 1
        jasi (i small pass 20) start
            make fact get factorial(i)
            make fib get fibonacci(i)
            if to say (i mod 3 na 0) start
                shout("Multiple of 3: ", i)
            end if not so start
                if to say (i mod 2 na 0) start
                    shout("Even: ", i)
                end if not so start
                    shout("Odd: ", i)
                end
            end
            i get i add 1
        end
    "#;
    let arena = scratch_arena(None);
    c.bench_function("parse_long_program", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parse_string_interpolation(c: &mut Criterion) {
    let src = r#"
        make name get "World"
        make greeting get "Hello {name}, how you dey?"
        shout(greeting)
    "#;
    let arena = scratch_arena(None);
    c.bench_function("parse_string_interpolation", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parse_nested_expr(c: &mut Criterion) {
    let src = "make result get ((((1 add 2) times 3) divide 4) minus 5) add ((6 times 7) divide (8 add 9))";
    let arena = scratch_arena(None);

    c.bench_function("parse_nested_expression", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src, &arena);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_string_slice(c: &mut Criterion) {
    let src = "Hello, 世界! 🌎";
    let arena = scratch_arena(None);

    c.bench_function("string_slice", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let s = builtins::string_slice(src, 0.0, Some(5.0), &arena);
            black_box(s);
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench(c: &mut Criterion) {
    arena::init(128 * MEBI).unwrap();

    bench_string_slice(c);
    bench_skip_comment(c);
    bench_scan_string(c);
    bench_parse_assignment(c);
    bench_parse_arithmetic_expr(c);
    bench_parse_fn_def(c);
    bench_parse_ctrl_flow(c);
    bench_parse_long_program(c);
    bench_parse_string_interpolation(c);
    bench_parse_nested_expr(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
