// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    punctuated::Punctuated, spanned::Spanned, token, Attribute, Data, DataEnum, DeriveInput, Error,
    Field, Fields, FieldsUnnamed, Ident, Lifetime, Meta, MetaList, MetaNameValue, NestedMeta,
    Result, Type, TypeReference, Variant, Visibility,
};

const MATCHER_ATTRIBUTE_NAME: &'static str = "matcher";
const MATCHER_TRAIT_ATTRIBUTE_NAME: &'static str = "as_trait";
const REF_MATCHER_ATTRIBUTE_NAME: &'static str = "as_ref";

pub fn execute(input: TokenStream) -> TokenStream {
    syn::parse(input)
        .and_then(|ast| {
            let options = parse_matcher_macro_options(&ast)?;
            create_matcher_impl(&ast, options)
        })
        .unwrap_or_else(|err| err.to_compile_error().into())
}

#[derive(Default, Clone, Debug)]
pub(crate) struct MatcherMacroOptions {
    pub matcher_traits: Vec<MatcherTraitOptions>,
    pub ref_matchers: Vec<RefMatcherOptions>,
}
pub(crate) enum MatcherMacroOption {
    MatcherTraits(Vec<MatcherTraitOptions>),
    RefMatchers(Vec<RefMatcherOptions>),
}

#[derive(Clone, Debug)]
pub(crate) struct MatcherTraitOptions {
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub(crate) struct RefMatcherOptions {
    pub name: Ident,
    pub derive_annotations: Vec<Ident>,
}

impl FromIterator<MatcherMacroOption> for MatcherMacroOptions {
    fn from_iter<T: IntoIterator<Item = MatcherMacroOption>>(iter: T) -> Self {
        iter.into_iter().fold(
            MatcherMacroOptions::default(),
            |mut combined_options, option| {
                match option {
                    MatcherMacroOption::MatcherTraits(options) => {
                        combined_options.matcher_traits.extend(options);
                    }
                    MatcherMacroOption::RefMatchers(options) => {
                        combined_options.ref_matchers.extend(options);
                    }
                }
                combined_options
            },
        )
    }
}
impl FromIterator<MatcherMacroOptions> for MatcherMacroOptions {
    fn from_iter<T: IntoIterator<Item = MatcherMacroOptions>>(iter: T) -> Self {
        iter.into_iter().fold(
            MatcherMacroOptions::default(),
            |mut combined_options, options| {
                let MatcherMacroOptions {
                    matcher_traits,
                    ref_matchers,
                } = options;
                combined_options.matcher_traits.extend(matcher_traits);
                combined_options.ref_matchers.extend(ref_matchers);
                combined_options
            },
        )
    }
}

fn create_matcher_impl(matcher: &DeriveInput, options: MatcherMacroOptions) -> Result<TokenStream> {
    match &matcher.data {
        Data::Enum(data) => create_enum_matcher_impl(matcher, data, options),
        Data::Struct(_) => Err(Error::new(matcher.span(), "Expected enum, received struct")),
        Data::Union(_) => Err(Error::new(matcher.span(), "Expected enum, received union")),
    }
}

fn create_enum_matcher_impl(
    matcher: &DeriveInput,
    variants: &DataEnum,
    options: MatcherMacroOptions,
) -> Result<TokenStream> {
    let MatcherMacroOptions {
        matcher_traits: matcher_trait_options,
        ref_matchers: ref_matcher_options,
    } = options;
    let variant_fields = parse_enum_variant_fields(variants.variants.iter())?;
    let matcher_traits = matcher_trait_options
        .into_iter()
        .map(|options| create_matcher_trait(options, &variant_fields))
        .collect::<Result<Vec<_>>>()?;
    let ref_matchers = ref_matcher_options
        .into_iter()
        .map(|options| create_ref_matcher(options, &variant_fields))
        .collect::<Result<Vec<_>>>()?;
    let matcher_impls = {
        let convert_impl = create_matcher_convert_impl(&matcher.ident, &variant_fields);
        let enum_variant_impls = variant_fields
            .into_iter()
            .map(|(variant, field)| {
                create_enum_variant_matcher_impl(&matcher.ident, &variant.ident, &field.ty)
            })
            .collect::<Result<Vec<_>>>()?;
        [convert_impl].into_iter().chain(enum_variant_impls)
    };
    Ok(TokenStream::from_iter(
        matcher_impls
            .into_iter()
            .chain(matcher_traits)
            .chain(ref_matchers.into_iter().flatten()),
    ))
}

fn parse_enum_variant_fields<'a>(
    variants: impl IntoIterator<Item = &'a Variant>,
) -> Result<Vec<(&'a Variant, &'a Field)>> {
    variants
        .into_iter()
        .map(|variant| parse_enum_variant_field(variant).map(|inner| (variant, inner)))
        .collect::<Result<Vec<_>>>()
}

fn parse_matcher_macro_options(matcher: &DeriveInput) -> Result<MatcherMacroOptions> {
    Ok(matcher
        .attrs
        .iter()
        .map(|attr| {
            parse_matcher_attribute(attr)
                .ok_or_else(|| Error::new(attr.span(), "Invalid macro attributes"))
                .and_then(|result| result)
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .collect::<MatcherMacroOptions>())
}

pub(crate) fn parse_matcher_attribute(attr: &Attribute) -> Option<Result<MatcherMacroOptions>> {
    attr.parse_meta().ok().and_then(|attr| match attr {
        Meta::Path(_) => None,
        Meta::NameValue(_) => None,
        Meta::List(attr) => attr.path.get_ident().and_then(|key| {
            if *key == MATCHER_ATTRIBUTE_NAME {
                Some(parse_matcher_attribute_option(&attr))
            } else {
                None
            }
        }),
    })
}

fn parse_matcher_attribute_option(attr: &MetaList) -> Result<MatcherMacroOptions> {
    let options = attr
        .nested
        .iter()
        .map(|attr| {
            match attr {
                NestedMeta::Lit(_) => None,
                NestedMeta::Meta(attr) => match attr {
                    Meta::Path(_) => None,
                    Meta::List(attr) => Some(parse_matcher_list_option(attr)),
                    Meta::NameValue(attr) => Some(parse_matcher_named_option(attr)),
                },
            }
            .ok_or_else(|| Error::new(attr.span(), "Invalid macro attribute"))
            .and_then(|result| result)
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(options.into_iter().collect::<MatcherMacroOptions>())
}

fn parse_matcher_list_option(attr: &MetaList) -> Result<MatcherMacroOption> {
    attr.path
        .get_ident()
        .map(|key| {
            if *key == MATCHER_TRAIT_ATTRIBUTE_NAME {
                parse_matcher_trait_option(&attr)
                    .map(|options| MatcherMacroOption::MatcherTraits(vec![options]))
            } else if *key == REF_MATCHER_ATTRIBUTE_NAME {
                parse_ref_matcher_name_option(&attr)
                    .map(|options| MatcherMacroOption::RefMatchers(vec![options]))
            } else {
                Err(Error::new(
                    key.span(),
                    format!("Invalid macro attribute: {}", key.to_string()),
                ))
            }
        })
        .ok_or_else(|| Error::new(attr.span(), "Invalid macro attribute"))
        .and_then(|result| result)
}

fn parse_matcher_named_option(attr: &MetaNameValue) -> Result<MatcherMacroOption> {
    attr.path
        .get_ident()
        .map(|key| {
            Err(Error::new(
                key.span(),
                format!("Invalid macro attribute: {}", key.to_string()),
            ))
        })
        .ok_or_else(|| Error::new(attr.span(), "Invalid macro attribute"))
        .and_then(|result| result)
}

fn parse_matcher_trait_option(attr: &MetaList) -> Result<MatcherTraitOptions> {
    parse_ident_list_option(attr)
        .and_then(|args| {
            let mut args = args.into_iter();
            let name = args.next()?;
            match args.next() {
                Some(_) => None,
                None => Some(MatcherTraitOptions { name }),
            }
        })
        .ok_or_else(|| Error::new(attr.span(), "Invalid trait name attribute value"))
}

fn parse_ref_matcher_name_option(attr: &MetaList) -> Result<RefMatcherOptions> {
    parse_ident_list_option(attr)
        .and_then(|args| {
            let mut args = args.into_iter();
            let name = args.next()?;
            let derive_annotations = args.collect::<Vec<_>>();
            Some(RefMatcherOptions {
                name,
                derive_annotations,
            })
        })
        .ok_or_else(|| Error::new(attr.span(), "Invalid ref matcher attribute value"))
}

fn parse_ident_list_option(attr: &MetaList) -> Option<Vec<Ident>> {
    attr.nested
        .iter()
        .map(|option| match option {
            NestedMeta::Meta(value) => match value {
                Meta::List(_) => None,
                Meta::NameValue(_) => None,
                Meta::Path(value) => value.get_ident().cloned(),
            },
            NestedMeta::Lit(_) => None,
        })
        .collect::<Option<Vec<_>>>()
}

pub fn parse_enum_variant_field(variant: &Variant) -> Result<&Field> {
    let mut enum_fields = variant.fields.iter();
    match enum_fields.next() {
        None => Err(Error::new(variant.span(), "Missing enum variant field")),
        Some(field) => match enum_fields.next() {
            Some(_) => Err(Error::new(variant.span(), "Multiple enum variant fields")),
            None => {
                if field.ident.is_some() {
                    Err(Error::new(
                        variant.span(),
                        "Matcher enum variants cannot contain named fields",
                    ))
                } else {
                    Ok(field)
                }
            }
        },
    }
}

fn create_matcher_convert_impl(name: &Ident, variant_fields: &[(&Variant, &Field)]) -> TokenStream {
    let variant_names = variant_fields
        .iter()
        .copied()
        .map(|(variant, _)| variant.ident.clone());
    let variant_types = variant_fields
        .iter()
        .copied()
        .map(|(_, variant_field)| variant_field.ty.clone());
    let output = quote! {
        impl #name {
            fn convert_into<T>(self) -> T
            where
                T: #(From<#variant_types>)+*,
            {
                match self {
                    #(#name::#variant_names(inner) => inner.into(),)*
                }
            }
        }
    }
    .into();
    output
}

fn create_enum_variant_matcher_impl(
    matcher_name: &Ident,
    variant_name: &Ident,
    field_type: &Type,
) -> Result<TokenStream> {
    let output = quote! {
        #[automatically_derived]
        impl From<#field_type> for #matcher_name {
            fn from(value: #field_type) -> Self {
                Self::#variant_name(value)
            }
        }
        #[automatically_derived]
        impl From<#matcher_name> for Option<#field_type> {
            fn from(value: #matcher_name) -> Self {
                match value {
                    #matcher_name::#variant_name(inner) => Some(inner),
                    _ => None,
                }
            }
        }
        #[automatically_derived]
        impl<'a> From<&'a #matcher_name> for Option<&'a #field_type> {
            fn from(value: &'a #matcher_name) -> Self {
                match value {
                    #matcher_name::#variant_name(inner) => Some(inner),
                    _ => None,
                }
            }
        }
        #[automatically_derived]
        impl From<#matcher_name> for Result<#field_type, #matcher_name> {
            fn from(value: #matcher_name) -> Self {
                match value {
                    #matcher_name::#variant_name(inner) => Ok(inner),
                    unmatched => Err(unmatched),
                }
            }
        }
    }
    .into();
    Ok(output)
}

fn create_matcher_trait(
    options: MatcherTraitOptions,
    variant_fields: &[(&Variant, &Field)],
) -> Result<TokenStream> {
    let MatcherTraitOptions { name } = options;
    let variant_types = variant_fields
        .iter()
        .copied()
        .map(|(_, variant_field)| variant_field.ty.clone());
    let span = name.span();
    let output = {
        let variant_types = variant_types.collect::<Vec<_>>();
        let variant_types_1 = variant_types.iter();
        let variant_types_2 = variant_types.iter();
        let variant_types_3 = variant_types.iter();
        let variant_types_4 = variant_types.iter();
        let variant_types_5 = variant_types.iter();
        let variant_types_6 = variant_types.iter();
        let variant_types_7 = variant_types.iter();
        let variant_types_8 = variant_types.iter();
        quote_spanned! {span=>
            trait #name where
                #(Self: From<#variant_types_1>,)*
                #(Option<#variant_types_2>: From<Self>,)*
                #(for<'a> Option<&'a #variant_types_3>: From<&'a Self>,)*
                #(Result<#variant_types_4, Self>: From<Self>,)*
                {}
            impl<T> #name for T where
                #(Self: From<#variant_types_5>,)*
                #(Option<#variant_types_6>: From<Self>,)*
                #(for<'a> Option<&'a #variant_types_7>: From<&'a Self>,)*
                #(Result<#variant_types_8, Self>: From<Self>,)*
                {}
        }
    }
    .into();
    Ok(output)
}

fn create_ref_matcher(
    options: RefMatcherOptions,
    variant_fields: &[(&Variant, &Field)],
) -> Result<Vec<TokenStream>> {
    let RefMatcherOptions {
        name,
        derive_annotations,
    } = options;
    let variant_definitions = variant_fields
        .iter()
        .copied()
        .map(|(variant, variant_field)| {
            let variant_type = &variant_field.ty;
            Variant {
                attrs: Vec::new(),
                ident: variant.ident.clone(),
                fields: Fields::Unnamed(FieldsUnnamed {
                    paren_token: token::Paren::default(),
                    unnamed: Punctuated::from_iter([Field {
                        attrs: Default::default(),
                        vis: Visibility::Inherited,
                        ident: None,
                        colon_token: None,
                        ty: Type::Reference(TypeReference {
                            and_token: token::And::default(),
                            lifetime: Some(Lifetime::new("'a", variant_type.span())),
                            mutability: None,
                            elem: Box::new(variant_field.ty.clone()),
                        }),
                    }]),
                }),
                discriminant: None,
            }
        });
    let variant_names = variant_fields
        .iter()
        .copied()
        .map(|(variant, _)| variant.ident.clone());
    let variant_types = variant_fields
        .iter()
        .copied()
        .map(|(_, variant_field)| variant_field.ty.clone());
    let matcher_impls = variant_fields
        .iter()
        .copied()
        .map(|(variant, field)| {
            create_enum_variant_ref_matcher_impl(&name, &variant.ident, &field.ty)
        })
        .collect::<Result<Vec<_>>>()?;
    let span = name.span();
    let output = quote_spanned! {span=>
        #[derive(Clone, Copy #(,#derive_annotations)*)]
        enum #name<'a> {
            #(#variant_definitions),*
        }
        #[automatically_derived]
        impl<'a> #name<'a> {
            fn match_from<T>(value: &'a T) -> Option<Self>
            where
                #(for<'b> Option<&'b #variant_types>: From<&'b T>,)*
            {
                None
                    #(.or_else(|| Option::<&_>::from(value).map(Self::#variant_names)))*
            }
        }
    }
    .into();
    Ok([output].into_iter().chain(matcher_impls).collect())
}

fn create_enum_variant_ref_matcher_impl(
    matcher_name: &Ident,
    variant_name: &Ident,
    field_type: &Type,
) -> Result<TokenStream> {
    let output = quote! {
        #[automatically_derived]
        impl<'a> From<&'a #field_type> for #matcher_name<'a> {
            fn from(value: &'a #field_type) -> Self {
                Self::#variant_name(value)
            }
        }
        #[automatically_derived]
        impl<'a> From<#matcher_name<'a>> for Option<&'a #field_type> {
            fn from(value: #matcher_name<'a>) -> Self {
                match value {
                    #matcher_name::#variant_name(inner) => Some(inner),
                    _ => None,
                }
            }
        }
    }
    .into();
    Ok(output)
}
