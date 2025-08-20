use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use naijascript::arena::Arena;
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
            let lexer = Lexer::new(src);
            let token: Vec<_> = lexer.collect();
            black_box(token);
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
    c.bench_function("scan_string", |b| {
        b.iter(|| {
            let lexer = Lexer::new(src);
            let token: Vec<_> = lexer.collect();
            black_box(token);
        })
    });
}

fn bench_parser_basic_assignment(c: &mut Criterion) {
    let src = "make x get 42";
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_basic_assignment", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_arithmetic_expression(c: &mut Criterion) {
    let src = "make result get 1 add 2 times 3 divide 4 minus 5";
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_arithmetic_expression", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_function_definition(c: &mut Criterion) {
    let src = r#"
        do fibonacci(n) start
            if to say (n small pass 2) start
                return n
            end
            return fibonacci(n minus 1) add fibonacci(n minus 2)
        end
    "#;
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_function_definition", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_control_flow(c: &mut Criterion) {
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
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_control_flow", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_large_program(c: &mut Criterion) {
    let src = r#"
        # Large program with multiple functions and complex expressions
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
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_large_program", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_string_interpolation(c: &mut Criterion) {
    let src = r#"
        make name get "World"
        make greeting get "Hello {name}, how you dey?"
        shout(greeting)
    "#;
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_string_interpolation", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

fn bench_parser_nested_expressions(c: &mut Criterion) {
    let src = "make result get ((((1 add 2) times 3) divide 4) minus 5) add ((6 times 7) divide (8 add 9))";
    let arena = Arena::new(4 * 1024).unwrap();

    c.bench_function("parser_nested_expressions", |b| {
        b.iter(|| {
            let offset = arena.offset();
            let lexer = Lexer::new(src);
            let mut parser = Parser::new(lexer, &arena);
            let (root, error) = parser.parse_program();
            black_box((root, error));
            unsafe { arena.reset(offset) };
        })
    });
}

criterion_group!(
    benches,
    bench_skip_comment,
    bench_scan_string,
    bench_parser_basic_assignment,
    bench_parser_arithmetic_expression,
    bench_parser_function_definition,
    bench_parser_control_flow,
    bench_parser_large_program,
    bench_parser_string_interpolation,
    bench_parser_nested_expressions
);
criterion_main!(benches);
