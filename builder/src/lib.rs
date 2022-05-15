use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
  let _input = parse_macro_input!(input as syn::DeriveInput);
  TokenStream::new()
}
