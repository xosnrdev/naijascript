use naijascript::arena::ArenaCow;
use naijascript::diagnostics::AsStr;
use naijascript::helpers::MEBI;
use naijascript::runtime::{RuntimeErrorKind, Value};

mod common;
use common::with_stress_pipeline;

macro_rules! assert_stress {
    ($src:expr, [$($expected:expr),* $(,)?]) => {
        with_stress_pipeline($src, |_, (root, errs), resolver, runtime| {
            assert!(errs.diagnostics.is_empty(), "Parse errors: {:?}", errs.diagnostics);
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "Resolve errors: {:?}", resolver.errors.diagnostics);
            runtime.run(root);
            assert!(!runtime.errors.has_errors(), "Runtime errors: {:?}", runtime.errors.diagnostics);
            assert_eq!(runtime.output, vec![$($expected),*]);
        })
    };
}

// ---------------------------------------------------------------------------
// 01: String variable overwrite 10K iterations
// ---------------------------------------------------------------------------
mod string_overwrite_loop {
    use super::*;

    #[test]
    fn last_value_survives() {
        assert_stress!(
            include_str!("stress/01_string_overwrite_loop.ns"),
            [Value::Str(ArenaCow::borrowed("iteration_9999_payload"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 02: Function returning a new string each iteration
// ---------------------------------------------------------------------------
mod function_return_string_loop {
    use super::*;

    #[test]
    fn last_label_survives() {
        assert_stress!(
            include_str!("stress/02_function_return_string_loop.ns"),
            [Value::Str(ArenaCow::borrowed("label_9999"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 03: Nested function calls with forward references (semantic error)
// ---------------------------------------------------------------------------
mod nested_function_calls {
    use super::*;

    #[test]
    fn rejects_forward_reference() {
        with_stress_pipeline(
            include_str!("stress/03_nested_function_calls.ns"),
            |_, (root, errs), resolver, _| {
                assert!(errs.diagnostics.is_empty());
                resolver.resolve(root);
                assert!(
                    resolver.errors.has_errors(),
                    "Forward function references should produce semantic errors"
                );
            },
        );
    }
}

// ---------------------------------------------------------------------------
// 04: Progressive string concatenation (s = s + "x")
// ---------------------------------------------------------------------------
mod string_concat_growth {
    use super::*;

    #[test]
    fn final_length() {
        assert_stress!(include_str!("stress/04_string_concat_growth.ns"), [Value::Number(5000.0)]);
    }
}

// ---------------------------------------------------------------------------
// 05: Push N / pop all, 100 cycles
// ---------------------------------------------------------------------------
mod array_push_pop_cycle {
    use super::*;

    #[test]
    fn empty_after_cycles() {
        assert_stress!(
            include_str!("stress/05_array_push_pop_cycle.ns"),
            [Value::Number(0.0), Value::Number(100.0)]
        );
    }
}

// ---------------------------------------------------------------------------
// 06: String method chains (trim, upper, lower, find, replace, slice)
// ---------------------------------------------------------------------------
mod string_method_chains {
    use super::*;

    #[test]
    fn all_iterations_find_world() {
        assert_stress!(include_str!("stress/06_string_method_chains.ns"), [Value::Number(5000.0)]);
    }
}

// ---------------------------------------------------------------------------
// 07: String interpolation overwriting same variable
// ---------------------------------------------------------------------------
mod interpolation_overwrite {
    use super::*;

    #[test]
    fn final_greeting() {
        assert_stress!(
            include_str!("stress/07_interpolation_overwrite.ns"),
            [Value::Str(ArenaCow::borrowed("Hello user_9999, welcome to iteration 9999"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 08: Recursive fibonacci(25)
// ---------------------------------------------------------------------------
mod recursive_fibonacci {
    use super::*;

    #[test]
    fn fib_25() {
        assert_stress!(include_str!("stress/08_recursive_fibonacci.ns"), [Value::Number(75025.0)]);
    }
}

// ---------------------------------------------------------------------------
// 09: Intentional infinite recursion — must produce StackOverflow, not abort
// ---------------------------------------------------------------------------
mod stack_overflow_recovery {
    use super::*;

    // STACK_BUDGET (4 MiB) exceeds cargo test's default 2 MiB thread stack.
    // Spawns a dedicated thread so the runtime guard triggers before the OS.
    #[test]
    fn catches_overflow() {
        std::thread::Builder::new()
            .stack_size(8 * MEBI)
            .spawn(|| {
                with_stress_pipeline(
                    include_str!("stress/09_stack_overflow_recovery.ns"),
                    |_, (root, errs), resolver, runtime| {
                        assert!(errs.diagnostics.is_empty());
                        resolver.resolve(root);
                        assert!(!resolver.errors.has_errors());
                        runtime.run(root);
                        assert!(
                            runtime
                                .errors
                                .diagnostics
                                .iter()
                                .any(|e| e.message == RuntimeErrorKind::StackOverflow.as_str()),
                            "Expected StackOverflow, got: {:?}",
                            runtime.errors.diagnostics
                        );
                    },
                );
            })
            .unwrap()
            .join()
            .unwrap();
    }
}

// ---------------------------------------------------------------------------
// 10: Mutual recursion with forward references (semantic error)
// ---------------------------------------------------------------------------
mod mutual_recursion {
    use super::*;

    #[test]
    fn rejects_forward_reference() {
        with_stress_pipeline(
            include_str!("stress/10_mutual_recursion.ns"),
            |_, (root, errs), resolver, _| {
                assert!(errs.diagnostics.is_empty());
                resolver.resolve(root);
                assert!(
                    resolver.errors.has_errors(),
                    "Mutual forward references should produce semantic errors"
                );
            },
        );
    }
}

// ---------------------------------------------------------------------------
// 11: Triple-nested loops (50*50*50 = 125,000 iterations)
// ---------------------------------------------------------------------------
mod nested_loops_scoping {
    use super::*;

    #[test]
    fn total_iterations() {
        assert_stress!(
            include_str!("stress/11_nested_loops_scoping.ns"),
            [Value::Number(125_000.0)]
        );
    }
}

// ---------------------------------------------------------------------------
// 12: 100x100 2D array — verify corners and length
// ---------------------------------------------------------------------------
mod array_of_arrays {
    use super::*;

    #[test]
    fn matrix_corners() {
        assert_stress!(
            include_str!("stress/12_array_of_arrays.ns"),
            [
                Value::Number(0.0),
                Value::Number(99.0),
                Value::Number(9900.0),
                Value::Number(9999.0),
                Value::Number(100.0),
            ]
        );
    }
}

// ---------------------------------------------------------------------------
// 13: Split/join cycling 5,000 iterations
// ---------------------------------------------------------------------------
mod split_join_cycle {
    use super::*;

    #[test]
    fn round_trips_preserve_data() {
        assert_stress!(
            include_str!("stress/13_split_join_cycle.ns"),
            [Value::Str(ArenaCow::borrowed("alpha,bravo,charlie,delta,echo,foxtrot,golf,hotel"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 14: 8 variables all overwritten per iteration
// ---------------------------------------------------------------------------
mod many_variables {
    use super::*;

    #[test]
    fn first_and_last_survive() {
        assert_stress!(
            include_str!("stress/14_many_variables.ns"),
            [Value::Str(ArenaCow::borrowed("a_4999")), Value::Str(ArenaCow::borrowed("h_4999")),]
        );
    }
}

// ---------------------------------------------------------------------------
// 15: FizzBuzz to 10,000 — spot-check representative values
// ---------------------------------------------------------------------------
mod fizzbuzz_10k {
    use super::*;

    #[test]
    fn spot_check() {
        with_stress_pipeline(
            include_str!("stress/15_fizzbuzz_10k.ns"),
            |_, (root, errs), resolver, runtime| {
                assert!(errs.diagnostics.is_empty());
                resolver.resolve(root);
                assert!(!resolver.errors.has_errors());
                runtime.run(root);
                assert!(!runtime.errors.has_errors());

                let out = &runtime.output;
                assert_eq!(out.len(), 10_000, "FizzBuzz 1..10000 produces 10,000 outputs");

                // i=1 → 1
                assert_eq!(out[0], Value::Number(1.0));
                // i=3 → Fizz
                assert_eq!(out[2], Value::Str(ArenaCow::borrowed("Fizz")));
                // i=5 → Buzz
                assert_eq!(out[4], Value::Str(ArenaCow::borrowed("Buzz")));
                // i=15 → FizzBuzz
                assert_eq!(out[14], Value::Str(ArenaCow::borrowed("FizzBuzz")));
                // i=30 → FizzBuzz
                assert_eq!(out[29], Value::Str(ArenaCow::borrowed("FizzBuzz")));
                // i=97 → 97 (prime, not divisible by 3 or 5)
                assert_eq!(out[96], Value::Number(97.0));
                // i=10000 → Buzz (10000 % 5 == 0, 10000 % 3 != 0)
                assert_eq!(out[9999], Value::Str(ArenaCow::borrowed("Buzz")));
            },
        );
    }
}

// ---------------------------------------------------------------------------
// 16: Functions modifying outer-scope variables
// ---------------------------------------------------------------------------
mod closure_like_scoping {
    use super::*;

    #[test]
    fn counter_and_label() {
        assert_stress!(
            include_str!("stress/16_closure_like_scoping.ns"),
            [Value::Number(10_000.0), Value::Str(ArenaCow::borrowed("count_10000")),]
        );
    }
}

// ---------------------------------------------------------------------------
// 17: Variable changes type every iteration (string/number/bool/null/array)
// ---------------------------------------------------------------------------
mod type_juggling {
    use super::*;

    // i=9999, phase = 9999 % 5 = 4 → val = [9999, 10000, 10001] → typeof = "array"
    #[test]
    fn final_type() {
        assert_stress!(
            include_str!("stress/17_type_juggling.ns"),
            [Value::Str(ArenaCow::borrowed("array"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 18: Repeated string replacement toggle (the ↔ THE), 2,000 rounds
// ---------------------------------------------------------------------------
mod string_replace_loop {
    use super::*;

    #[test]
    fn round_trips_preserve_text() {
        assert_stress!(
            include_str!("stress/18_string_replace_loop.ns"),
            [Value::Str(ArenaCow::borrowed("the quick brown fox jumps over the lazy dog"))]
        );
    }
}

// ---------------------------------------------------------------------------
// 19: Overwriting array elements by index in a loop
// ---------------------------------------------------------------------------
mod array_index_write {
    use super::*;

    // Last writes: arr[0] at i=4990 ("val_4990"), arr[9] at i=4999 ("val_4999")
    #[test]
    fn final_elements() {
        assert_stress!(
            include_str!("stress/19_array_index_write.ns"),
            [
                Value::Str(ArenaCow::borrowed("val_4990")),
                Value::Str(ArenaCow::borrowed("val_4999")),
                Value::Number(10.0),
            ]
        );
    }
}

// ---------------------------------------------------------------------------
// 20: Combined torture — functions, arrays, strings, interpolation, methods
// ---------------------------------------------------------------------------
mod combined_torture {
    use super::*;

    // process(500) → 500, process(500) → 500, sum(1..1000) → 500500
    #[test]
    fn all_subsystems() {
        assert_stress!(
            include_str!("stress/20_combined_torture.ns"),
            [Value::Number(500.0), Value::Number(500.0), Value::Number(500_500.0),]
        );
    }
}
