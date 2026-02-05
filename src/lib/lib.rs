#![warn(clippy::all)]

// Suppress warnings about interior mutability used in keys
// since we use custom hashing that ignore the mutable fields.
#![allow(clippy::mutable_key_type)]

pub mod kernel;
pub mod runtime;
pub mod frontend;
pub mod utils;

pub mod includes;