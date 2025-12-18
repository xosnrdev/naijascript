#![feature(allocator_api, cold_path, new_range_api)]
#![cfg_attr(
    target_arch = "loongarch64",
    feature(stdarch_loongarch, stdarch_loongarch_feature_detection, loongarch_target_feature)
)]
#![allow(
    clippy::many_single_char_names,
    clippy::similar_names,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::too_many_lines
)]

#[macro_use]
pub mod arena;
pub mod builtins;
pub mod diagnostics;
pub mod helpers;
pub mod resolver;
pub mod runtime;
pub mod syntax;
pub mod sys;
