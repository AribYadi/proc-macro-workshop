use proc_macro::TokenStream;
use quote::{
  format_ident,
  quote,
};
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as syn::DeriveInput);

  let input_ident = input.ident;
  let input_fields = match input.data {
    syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(fields), .. }) => fields,
    _ => panic!("#[derive(Builder)] is only defined for structs"),
  };
  let input_field_idents =
    input_fields.named.iter().map(|field| field.ident.as_ref().unwrap()).collect::<Vec<_>>();
  let input_field_types =
    input_fields.named.iter().map(|field| field.ty.clone()).collect::<Vec<_>>();

  let builder_ident = format_ident!("{}Builder", input_ident);
  let builder_fields = get_builder_fields(&input_fields);

  quote! {
    pub struct #builder_ident {
      #builder_fields
    }

    impl #input_ident {
      pub fn builder() -> #builder_ident {
        #builder_ident {
          #(
            #input_field_idents: None
          ),*
        }
      }
    }

    impl #builder_ident {
      #(
        pub fn #input_field_idents(&mut self, #input_field_idents: #input_field_types) -> &mut Self {
          self.#input_field_idents = Some(#input_field_idents);
          self
        }
      )*
    }
  }
  .into()
}

fn get_builder_fields(input_fields: &syn::FieldsNamed) -> quote::__private::TokenStream {
  let idents =
    input_fields.named.iter().map(|field| field.ident.clone().unwrap()).collect::<Vec<_>>();
  let types = input_fields.named.iter().map(|field| field.ty.clone()).collect::<Vec<_>>();
  let vis = input_fields.named.iter().map(|field| field.vis.clone()).collect::<Vec<_>>();

  quote! {
    #(#vis #idents: Option<#types>),*
  }
}
