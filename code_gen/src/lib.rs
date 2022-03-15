#![warn(clippy::all, clippy::pedantic, clippy::restriction)]
#![allow(
    clippy::missing_docs_in_private_items,
    clippy::implicit_return,
    clippy::shadow_reuse,
    clippy::wildcard_enum_match_arm,
    clippy::else_if_without_else,
    clippy::integer_arithmetic,
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::as_conversions,
    clippy::self_named_module_files,
    clippy::missing_inline_in_public_items,
    clippy::pattern_type_mismatch,
    clippy::must_use_candidate,
    clippy::float_arithmetic,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::indexing_slicing,
    clippy::missing_panics_doc,
    clippy::string_add,
    clippy::cast_possible_truncation
)]

pub mod code_gen;
pub mod lift;
pub mod wasm;
