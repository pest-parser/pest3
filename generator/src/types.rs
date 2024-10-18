use proc_macro2::TokenStream;
use quote::quote;

pub fn pest() -> TokenStream {
    quote! {
        ::pest3_core
    }
}
pub fn _str() -> TokenStream {
    quote! {::core::primitive::str}
}
pub fn option_type() -> TokenStream {
    let this = pest();
    quote! {#this::std::Option}
}
