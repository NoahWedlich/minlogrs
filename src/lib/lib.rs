#![warn(clippy::all)]

// Suppress warnings about interior mutability used in keys
// since we use custom hashing that ignore the mutable fields.
#![allow(clippy::mutable_key_type)]

pub mod utils;
pub mod core;
pub mod builtin;
pub mod proof_generation;
pub mod frontend;

pub mod includes;