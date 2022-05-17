use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as syn::DeriveInput);

  let input_ident = &input.ident;
  let input_fields = match input.data {
    syn::Data::Struct(syn::DataStruct {
      fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
      ..
    }) => named,
    _ => panic!("#[derive(CustomDebug)] only works with structs with named fields"),
  };
  let input_field_idents =
    input_fields.iter().map(|field| field.ident.as_ref().unwrap()).collect::<Vec<_>>();

  quote! {
    impl std::fmt::Debug for #input_ident {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(#input_ident))
          #(.field(stringify!(#input_field_idents), &self.#input_field_idents))*
          .finish()
      }
    }
  }
  .into()
}
