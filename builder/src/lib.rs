use proc_macro::TokenStream;
use quote::{
  format_ident,
  quote,
};
use syn::parse_macro_input;

#[proc_macro_derive(Builder, attributes(builder))]
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
    .map(|field| {
      if is_option(&field.ty) || has_builder_attr(field) {
        get_inner_type(field)
      } else {
        field.ty.clone()
      }
    })
    .collect::<Vec<_>>();

  let builder_ident = format_ident!("{}Builder", input_ident);
  let builder_fields = get_builder_fields(&input_fields);
  let builder_build_into = input_fields
    .named
    .iter()
    .map(|field| {
      if is_option(&field.ty) || has_builder_attr(field) {
        quote! {}
      } else {
        ".ok_or(format!(\"`{}` is not set!\", stringify!(#input_field_idents)))?"
          .parse::<syn::__private::TokenStream2>()
          .unwrap()
      }
    })
    .collect::<Vec<_>>();

  let mut errored = None;
  let field_fn_idents = input_fields
    .named
    .iter()
    .map(|field| {
      if has_builder_attr(field) {
        match get_builder_attr_each(&field.attrs) {
          Ok(a) => format_ident!("{}", a),
          Err(e) => {
            errored = Some(quote! { #e });
            format_ident!("_")
          },
        }
      } else {
        field.ident.clone().unwrap()
      }
    })
    .collect::<Vec<_>>();
  if let Some(e) = errored {
    return e.into();
  }
  let builder_set_field = input_fields
    .named
    .iter()
    .map(|field| {
      if has_builder_attr(field) {
        quote! {
          .push
        }
      } else {
        quote! {
           = Some
        }
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
            #input_field_idents: Default::default()
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
        pub fn #field_fn_idents(&mut self, #field_fn_idents: #input_field_types) -> &mut Self {
          self.#input_field_idents #builder_set_field(#field_fn_idents);
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
      if is_option(&ty) || has_builder_attr(field) {
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
      segments.iter().any(|segment| segment.ident == "Option")
    },
    _ => false,
  }
}

fn is_vec(ty: &syn::Type) -> bool {
  match ty {
    syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
      segments.iter().any(|segment| segment.ident == "Vec")
    },
    _ => false,
  }
}

fn get_inner_type(field: &syn::Field) -> syn::Type {
  let ty = &field.ty;
  if !(is_option(ty) || has_builder_attr(field)) {
    unreachable!("`get_inner_type` has been called with an argument of not type `Option` || `Vec with 'builder' attribute`");
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

fn has_builder_attr(field: &syn::Field) -> bool {
  let attrs = &field.attrs;
  is_vec(&field.ty) && attrs.iter().any(|attr| attr.path.is_ident("builder"))
}

fn get_builder_attr_each(attrs: &[syn::Attribute]) -> Result<String, syn::__private::TokenStream2> {
  let attr = attrs.iter().find(|attr| attr.path.is_ident("builder")).ok_or_else(|| {
    syn::Error::new(syn::__private::Span::call_site(), "`builder` attribute must be present")
      .into_compile_error()
  })?;

  match attr.parse_meta() {
    Ok(meta) => match meta {
      syn::Meta::List(ref metalist) => match metalist.nested.first() {
        Some(syn::NestedMeta::Meta(syn::Meta::NameValue(attr)))
          if attr.path.is_ident("each") && matches!(attr.lit, syn::Lit::Str(_)) =>
        {
          if let syn::Lit::Str(ref s) = attr.lit {
            return Ok(s.value());
          }

          unreachable!();
        },
        _ => Err(
          syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`").into_compile_error(),
        ),
      },
      _ => Err(
        syn::Error::new_spanned(meta, "expected `builder(each = \"...\")`").into_compile_error(),
      ),
    },
    Err(e) => Err(e.into_compile_error()),
  }
}
