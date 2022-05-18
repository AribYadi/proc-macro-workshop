use proc_macro::TokenStream;
use quote::quote;
use syn::{
  parse_macro_input,
  parse_quote,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as syn::DeriveInput);

  let input_ident = &input.ident;
  let input_fields = match input.data {
    syn::Data::Struct(syn::DataStruct {
      fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
      ..
    }) => named,
    _ => {
      return syn::Error::new(
        syn::__private::Span::call_site(),
        "`CustomDebug` only works for structs with named fields",
      )
      .into_compile_error()
      .into()
    },
  };
  let input_field_idents =
    input_fields.iter().map(|field| field.ident.as_ref().unwrap()).collect::<Vec<_>>();
  let format_types = match get_format_types(input_fields.iter().cloned().collect()) {
    Ok(types) => types,
    Err(err) => {
      return err.into();
    },
  };

  let input_generics = add_debug_bounds(input.generics);
  let (impl_generics, ty_generics, where_clause) = input_generics.split_for_impl();

  quote! {
    impl #impl_generics std::fmt::Debug for #input_ident #ty_generics #where_clause {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(#input_ident))
          #(.field(stringify!(#input_field_idents), &format_args!(#format_types, self.#input_field_idents)))*
          .finish()
      }
    }
  }
  .into()
}

fn get_format_types(
  input_fields: Vec<syn::Field>,
) -> Result<Vec<syn::LitStr>, syn::__private::TokenStream2> {
  fn get_format_type(field: &syn::Field) -> Result<syn::LitStr, syn::__private::TokenStream2> {
    match field.attrs.iter().find(|attr| attr.path.is_ident("debug")) {
      Some(attr) => match attr.parse_meta() {
        Ok(syn::Meta::NameValue(syn::MetaNameValue { lit, .. })) => match lit {
          syn::Lit::Str(s) => Ok(s),
          _ => Err(
            syn::Error::new_spanned(attr.tokens.clone(), "`debug` only works with string literals")
              .into_compile_error(),
          ),
        },
        _ => Err(
          syn::Error::new_spanned(attr, "Unexpected usage of `debug` attribute")
            .into_compile_error(),
        ),
      },
      None => Ok(parse_quote!("{:?}")),
    }
  }

  let mut format_types = Vec::new();
  for field in input_fields {
    let format_type = get_format_type(&field)?;
    format_types.push(format_type);
  }
  Ok(format_types)
}

fn add_debug_bounds(mut generics: syn::Generics) -> syn::Generics {
  for param in generics.params.iter_mut() {
    if let syn::GenericParam::Type(ref mut type_param) = param {
      type_param.bounds.push(parse_quote!(std::fmt::Debug));
    }
  }
  generics
}
