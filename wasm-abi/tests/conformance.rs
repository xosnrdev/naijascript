//! Cross-platform conformance harness.
//!
//! Each fixture directory under `conformance/fixtures/` holds a `.nui` source,
//! an optional `params.json`, an `events.json` selector script, and a committed
//! `golden.json`. This test runs every fixture through the engine and asserts
//! the golden is reproduced byte-for-byte (structurally). Reference renderers on
//! every platform replay the same golden: apply `mount`, then each `patch`, and
//! compare the resulting tree to `final_tree`.
//!
//! Regenerate goldens after an intentional protocol change with:
//! `REGEN_CONFORMANCE=1 cargo test -p naijascript-wasm-abi --test conformance`.

use std::fs;
use std::path::{Path, PathBuf};

use naijascript_wasm_abi::ui::conformance_run;
use serde_json::Value;

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("conformance").join("fixtures")
}

fn read_json(path: &Path) -> Value {
    let text =
        fs::read_to_string(path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    serde_json::from_str(&text).unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

#[test]
fn fixtures_match_committed_goldens() {
    let regen = std::env::var_os("REGEN_CONFORMANCE").is_some();
    let dir = fixtures_dir();
    let mut checked = 0;

    let mut entries: Vec<_> = fs::read_dir(&dir)
        .unwrap_or_else(|error| panic!("read fixtures dir {}: {error}", dir.display()))
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect();
    entries.sort();

    for fixture in entries {
        let source = fs::read_to_string(fixture.join("source.nui"))
            .unwrap_or_else(|error| panic!("read source for {}: {error}", fixture.display()));
        let params_path = fixture.join("params.json");
        let params = if params_path.exists() { read_json(&params_path) } else { Value::Null };
        let events = read_json(&fixture.join("events.json"));

        let actual = conformance_run(&source, &params, &events);
        let golden_path = fixture.join("golden.json");

        if regen || !golden_path.exists() {
            let pretty = serde_json::to_string_pretty(&actual).unwrap();
            fs::write(&golden_path, format!("{pretty}\n")).unwrap();
            continue;
        }

        let expected = read_json(&golden_path);
        assert_eq!(
            actual,
            expected,
            "fixture `{}` diverged from its golden; rerun with REGEN_CONFORMANCE=1 if intended",
            fixture.file_name().and_then(|name| name.to_str()).unwrap_or("?"),
        );
        checked += 1;
    }

    if !regen {
        assert!(checked > 0, "no conformance fixtures were checked");
    }
}
