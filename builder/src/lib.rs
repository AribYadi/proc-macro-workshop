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
  let input_field_types = input_fields
    .named
    .iter()
    .map(|field| if is_option(&field.ty) { get_option_type(&field.ty) } else { field.ty.clone() })
    .collect::<Vec<_>>();

  let builder_ident = format_ident!("{}Builder", input_ident);
  let builder_fields = get_builder_fields(&input_fields);
  let builder_build_into = input_fields
    .named
    .iter()
    .map(|field| {
      if is_option(&field.ty) {
        quote! {}
      } else {
        ".ok_or(format!(\"`{}` is not set!\", stringify!(#input_field_idents)))?"
          .parse::<syn::__private::TokenStream2>()
          .unwrap()
      }
    })
    .collect::<Vec<_>>();

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
      pub fn build(&mut self) -> Result<#input_ident, Box<dyn std::error::Error>> {
        Ok(#input_ident {
          #(
            #input_field_idents: self.#input_field_idents.clone()#builder_build_into
          ),*
        })
      }

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
  let types = input_fields
    .named
    .iter()
    .map(|field| {
      let ty = field.ty.clone();
      if is_option(&ty) {
        quote! { #ty }
      } else {
        quote! { Option<#ty> }
      }
    })
    .collect::<Vec<_>>();
  let vis = input_fields.named.iter().map(|field| field.vis.clone()).collect::<Vec<_>>();

  quote! {
    #(#vis #idents: #types),*
  }
}

fn is_option(ty: &syn::Type) -> bool {
  match ty {
    syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
      segments.len() == 1 && segments[0].ident == "Option"
    },
    _ => false,
  }
}

fn get_option_type(ty: &syn::Type) -> syn::Type {
  if !is_option(ty) {
    unreachable!("`get_option_type` has been called with an argument of not type `Option`");
  }
  match ty {
    syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
      match segments[0].arguments {
        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
          ref args,
          ..
        }) => match args[0] {
          syn::GenericArgument::Type(ref ty) => ty.clone(),
          _ => unreachable!(),
        },
        _ => unreachable!(),
      }
    },
    _ => unreachable!(),
  }
}
