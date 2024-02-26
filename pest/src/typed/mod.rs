//! Definitions related to typed syntax tree.

pub mod template;
mod tracker;
mod traits;
pub mod wrapper;

pub use tracker::Tracker;
pub use traits::{
    EmptyPairContainer, FullRuleStruct, NeverFailedTypedNode, PairContainer, PairTree, RuleType,
    TypedNode, TypedParser,
};
