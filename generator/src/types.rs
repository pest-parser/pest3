use proc_macro2::TokenStream;
use quote::quote;

pub fn pest() -> TokenStream {
    #[cfg(feature = "meta")]
    quote! {
        ::pest3_core
    }
    #[cfg(not(feature = "meta"))]
    quote! {
        ::pest3
    }
}
pub fn _str() -> TokenStream {
    quote! {::core::primitive::str}
}
pub fn option_type() -> TokenStream {
    let this = pest();
    quote! {#this::std::Option}
}
