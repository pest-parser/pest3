use proc_macro2::TokenStream;
use quote::quote;

pub fn _str() -> TokenStream {
    quote! {::core::primitive::str}
}
pub fn option_type() -> TokenStream {
    quote! {::core::option::Option}
}
