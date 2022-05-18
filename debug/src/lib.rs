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

  let input_generics = get_generics(&input_fields, input.generics);
  let (_, ty_generics, where_clause) = input_generics.split_for_impl();

  quote! {
    impl #ty_generics std::fmt::Debug for #input_ident #ty_generics #where_clause {
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

fn get_generics(
  fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
  mut generics: syn::Generics,
) -> syn::Generics {
  let generic_params = generics.params.clone();
  let where_clause = generics.make_where_clause();

  for generic_ident in generic_params.iter().filter_map(|generic| match generic {
    syn::GenericParam::Type(syn::TypeParam { ident, .. }) => Some(ident),
    _ => None,
  }) {
    where_clause.predicates.push(parse_quote!(#generic_ident: std::fmt::Debug));
  }

  for field_ty in fields.iter().filter_map(|field| match field.ty {
    syn::Type::Path(ref ty) => Some(ty.path.segments.last().unwrap()),
    _ => None,
  }) {
    if field_ty.ident == "PhantomData" {
      let field_ty_generic_ident = match field_ty.arguments {
        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
          ref args,
          ..
        }) => match args.first().unwrap() {
          syn::GenericArgument::Type(syn::Type::Path(ty)) => ty.path.get_ident().unwrap(),
          _ => unreachable!(),
        },
        _ => unreachable!(),
      };

      if let Some(bounded_ty) =
        where_clause.predicates.iter_mut().find_map(|predicate| match predicate {
          syn::WherePredicate::Type(syn::PredicateType { bounded_ty, .. }) => {
            if let syn::Type::Path(syn::TypePath { ref path, .. }) = bounded_ty {
              if path.is_ident(field_ty_generic_ident) {
                Some(bounded_ty)
              } else {
                None
              }
            } else {
              None
            }
          },
          _ => None,
        })
      {
        *bounded_ty = parse_quote!(std::marker::PhantomData<#field_ty_generic_ident>);
        continue;
      }

      where_clause
        .predicates
        .push(parse_quote!(std::marker::PhantomData<#field_ty_generic_ident>: std::fmt::Debug));
    }
  }

  generics
}
