#![warn(clippy::all, clippy::nursery, clippy::pedantic)]
#![allow(clippy::wildcard_imports, clippy::too_many_lines)]
#![allow(missing_docs, clippy::missing_panics_doc, clippy::missing_errors_doc)]

mod evaluator;
mod lexer;
mod parser;
mod program;
mod types;

pub use self::evaluator::*;
pub use self::lexer::*;
pub use self::parser::*;
pub use self::program::*;
pub use self::types::*;
