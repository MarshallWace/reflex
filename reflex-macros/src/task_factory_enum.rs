// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use proc_macro::TokenStream;

use crate::utils::{create_generic_arguments_for_params, parse_item_enum, parse_item_impl};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{Attribute, Block, Field, GenericParam, ItemImpl, PathArguments, Type, WhereClause};
use syn::{Fields, GenericArgument, Generics, Ident, ItemEnum, Variant, Visibility};

const TASK_FACTORY_TRAIT_NAME: &'static str = "TaskFactory";

/// This function generates a great deal of boiler plate.
/// By giving a task factory enum which is a thin wrapper around other task factory impls
/// we can generate:
///        task factory enum that consists of TaskFactory subtypes
///        a blanket_impl trait on all the types of action
///        an delegating impl of Named (optional?)
///        an impl of TaskFactory
///        An actor type that consists of actors of the TaskFactory subtypes
///        this delegates to the enum types for create since they must also be TaskFactory
///        a delegating impl of Named for the actor
///        An implementation of Actor for the Actor type
///        an Events type and a Dispose Type
///       it delegates init to its subtypes
///       An implementation of Worker which delegates to subtypes
///       An implementation of Handler that delegates
///       State type that is an enum of the subtypes states
///       A Events type that has pin_projects on it
///       delegating impl of stream for the events type
///       A dispose type that is an enum of the subtypes
///       delegating impl of dispose for the events type
///       For each of the subtypes a From implementation wrapping in the super type.
pub fn execute(input: TokenStream) -> TokenStream {
    syn::parse(input.clone())
        .and_then(|input: TaskFactoryConfiguration| produce_task_factory(input))
        .unwrap_or_else(|err| err.to_compile_error().into())
}

struct GenericsComponents {
    params: Punctuated<GenericParam, Comma>,
    args: Punctuated<GenericArgument, Comma>,
    where_clause: Option<WhereClause>,
}

impl From<Generics> for GenericsComponents {
    fn from(generics: Generics) -> Self {
        let params = generics.params;
        let args = create_generic_arguments_for_params(&params).collect();
        let where_clause = generics.where_clause;
        Self {
            params,
            args,
            where_clause,
        }
    }
}

struct TaskFactoryVariantComponents {
    raw_variants: Punctuated<Variant, Comma>,
    parsed_variants: Vec<(Span, Ident, Field)>,
}

impl TryFrom<Punctuated<Variant, Comma>> for TaskFactoryVariantComponents {
    type Error = syn::Error;

    fn try_from(value: Punctuated<Variant, Comma>) -> Result<Self, Self::Error> {
        let raw_variants = value.clone();
        let parsed_variants = value
            .into_iter()
            .map(|variant| {
                let span = variant.ident.span();
                let variant_name = variant.ident;
                let inner_factory = match variant.fields {
                    Fields::Unnamed(field) => field,
                    _ => {
                        return Err(syn::Error::new(
                            span,
                            "All enum variant items must be unnamed field variants",
                        ))
                    }
                };
                let field = inner_factory.unnamed[0].clone();
                Ok((span, variant_name, field))
            })
            .collect::<syn::Result<Vec<_>>>()?;
        Ok(TaskFactoryVariantComponents {
            raw_variants,
            parsed_variants,
        })
    }
}

struct TaskFactoryConfiguration {
    factory_ident: Ident,
    factory_generics: GenericsComponents,
    factory_variants: TaskFactoryVariantComponents,
    factory_vis: Visibility,
    factory_attrs: Vec<Attribute>,
    actor_ident: Ident,
    state_ident: Ident,
    events_ident: Ident,
    dispose_ident: Ident,
    ttask_taction_generics: GenericsComponents,
    taction: Type,
    ttask: Type,
}

impl Parse for TaskFactoryConfiguration {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let block: Block = input.parse()?;
        let span = block.span();
        let mut block_stmts = block.stmts.into_iter();

        let factory_enum = parse_item_enum(&mut block_stmts, span)?;
        let ItemEnum {
            ident: factory_ident,
            generics: factory_generics,
            variants: factory_variants,
            vis: factory_vis,
            attrs: factory_attrs,
            ..
        } = factory_enum;

        let mut dummy_factory_impl = parse_item_impl(&mut block_stmts, span)?;
        let (taction, ttask) = parse_taction_ttask(&mut dummy_factory_impl)?;
        let generics_with_ttask_taction = dummy_factory_impl.generics;

        let span = factory_ident.span();
        let factory_name = factory_ident.to_string();
        let factory_stem = factory_name
            .strip_suffix("Factory")
            .unwrap_or_else(|| factory_name.as_str());

        let mut actor_ident = format_ident!("{}Actor", factory_stem);
        actor_ident.set_span(span);
        let mut state_ident = format_ident!("{}State", actor_ident);
        state_ident.set_span(span);
        let mut events_ident = format_ident!("{}Events", actor_ident);
        events_ident.set_span(span);
        let mut dispose_ident = format_ident!("{}Dispose", actor_ident);
        dispose_ident.set_span(span);

        Ok(Self {
            factory_ident,
            factory_generics: factory_generics.into(),
            factory_variants: factory_variants.try_into()?,
            factory_vis,
            factory_attrs,
            actor_ident,
            state_ident,
            events_ident,
            dispose_ident,
            ttask_taction_generics: generics_with_ttask_taction.into(),
            taction,
            ttask,
        })
    }
}

fn produce_task_factory(input: TaskFactoryConfiguration) -> syn::Result<TokenStream> {
    let task_factory_named_impl = named_impl_for_task_factory(&input);
    let from_impls = impl_of_from_for_task_factory_variants(&input);
    let actor = actor_for_factory(&input);
    let actor_named_impl = named_for_actor(&input);
    let handle = impl_handler_for_actor(&input);
    let worker = impl_worker_for_actor(&input);
    let state = state_for_actor(&input);
    let dispose = dispose_for_actor(&input);
    let events = events_for_actor(&input);
    let actor_impl = impl_actor_for_actor(&input);
    let task_factory_impl = impl_of_task_factory_for_task_factory(&input);

    let span = input.factory_ident.span();
    let TaskFactoryConfiguration {
        factory_ident,
        factory_generics,
        factory_vis,
        factory_variants,
        factory_attrs,
        ..
    } = &input;
    let factory_variants = &factory_variants.raw_variants;
    let generic_parameters = &factory_generics.params;
    let where_clause = &factory_generics.where_clause;
    let ret = quote_spanned! {
        span =>

        #[allow(dead_code)]
        #(#factory_attrs)*
        #factory_vis enum #factory_ident<#generic_parameters> #where_clause {
            #factory_variants
        }

        #task_factory_named_impl
        #from_impls
        #actor
        #dispose
        #events
        #state
        #handle
        #worker
        #actor_impl
        #actor_named_impl
        #task_factory_impl
    };
    println!("{}", ret.to_string());
    Ok(ret.into())
}

fn named_impl_for_task_factory(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let TaskFactoryConfiguration {
        factory_ident,
        factory_generics,
        factory_variants,
        ..
    } = input;
    let GenericsComponents {
        params,
        where_clause,
        args,
    } = factory_generics;
    let mut named_delegates = TokenStream2::new();
    factory_variants
        .parsed_variants
        .iter()
        .for_each(|(span, ident, _)| {
            named_delegates.extend(quote_spanned! {
                *span =>
                Self::#ident(inner) => inner.name()
            });
        });

    quote_spanned! {span =>
        impl<#params> ::reflex_dispatcher::Named for #factory_ident<#args> #where_clause {
                fn name(&self) -> &'static str {
                    match self {
                        #named_delegates
                    }
                }
            }
    }
}

fn impl_of_from_for_task_factory_variants(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let GenericsComponents {
        params,
        where_clause,
        args,
    } = &input.factory_generics;
    let factory_ident = &input.factory_ident;
    let mut ret = TokenStream2::new();
    input
        .factory_variants
        .parsed_variants
        .iter()
        .for_each(|(span, variant_name, field)| {
            ret.extend(quote_spanned! {*span =>
                impl<#params> ::std::convert::From<#field> for #factory_ident<#args> #where_clause {
                   fn from(value: #field)  -> Self {
                        Self::#variant_name(value)
                    }
                }
            })
        });

    ret
}

fn impl_of_task_factory_for_task_factory(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params: gen_params,
        where_clause,
        args: gen_args,
    } = &input.ttask_taction_generics;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let gen_args_no_ttask = &input.factory_generics.args;
    let actor_ident = &input.actor_ident;
    let factory_ident = &input.factory_ident;
    let create_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                Self::#variant_name(inner) => #actor_ident::#variant_name(
                    < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::create(inner)
                    )
            }
        })
        .collect();

    quote_spanned! { span =>
        impl<#gen_params> ::reflex_dispatcher::TaskFactory<#taction,  #ttask> for #factory_ident<#gen_args_no_ttask> #where_clause {
            type Actor = #actor_ident<#gen_args>;
            fn create(self) -> Self::Actor {
                match self {
                    #(#create_variants),*
                }
            }
        }
    }
}

fn actor_for_factory(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params,
        where_clause,
        ..
    } = &input.ttask_taction_generics;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let actor_name = &input.actor_ident;
    let visibility = &input.factory_vis;
    let factory_attrs = &input.factory_attrs;
    let actor_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                #variant_name(<#field as ::reflex_dispatcher::TaskFactory<#taction, #ttask>>::Actor)
            }
        })
        .collect();
    quote_spanned! {
        span =>
        #(#factory_attrs)*
        #visibility enum #actor_name<#params> #where_clause {
            #(#actor_variants),*
        }
    }
}

fn impl_actor_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params,
        where_clause,
        args,
    } = &input.ttask_taction_generics;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let actor_ident = &input.actor_ident;
    let state_ident = &input.state_ident;
    let events_ident = &input.events_ident;
    let dispose_ident = &input.dispose_ident;
    let init_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span,variant_name,field)| {
            quote_spanned! {*span =>
                Self::#variant_name(actor) => {
                    let state =
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Actor<#taction, #ttask>>::init(actor);
                        #state_ident::#variant_name(state)
                }
                }
        })
        .collect();
    let events_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span,variant_name,field)| {
            quote_spanned! {*span =>
                Self::#variant_name(actor) => {
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Actor<#taction, #ttask>>::events(actor, inbox)
                    .map(|(events, dispose)| {
                        (
                        #events_ident::#variant_name(events),
                        dispose.map(#dispose_ident::#variant_name),
                        )
                    })
                }
                }
        })
        .collect();

    quote_spanned! { span =>
        impl<#params> ::reflex_dispatcher::Actor<#taction,  #ttask> for #actor_ident<#args> #where_clause {
            type Events<TInbox: ::reflex_dispatcher::TaskInbox<#taction>> = #events_ident<#args, TInbox>;
            type Dispose = #dispose_ident<#args>;

            fn init(
                &self,
            ) -> Self::State {
                match self {
                    #(#init_variants),*
                }
            }

            fn events<TInbox: ::reflex_dispatcher::TaskInbox<#taction>>(
                &self,
                inbox: TInbox,
            ) -> ::reflex_dispatcher::ActorEvents<TInbox, Self::Events<TInbox>, Self::Dispose> {
               match self {
                    #(#events_variants),*
                }
            }
        }
    }
}

fn impl_worker_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params,
        where_clause,
        args,
    } = &input.ttask_taction_generics;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let actor_ident = &input.actor_ident;
    let state_ident = &input.state_ident;
    let accept_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                Self::#variant_name(inner) => {
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Worker<#taction, ::reflex_dispatcher::SchedulerTransition<#taction,#ttask>>>::accept(inner, message)
                }
                }
        })
        .collect();

    let schedule_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                (Self::#variant_name(actor), #state_ident::#variant_name(state)) => {
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Worker<#taction, ::reflex_dispatcher::SchedulerTransition<#taction,#ttask>>>::schedule(actor, message, state)
                }
                }
        })
        .collect();

    quote_spanned! { span =>
        impl<#params> ::reflex_dispatcher::Worker<#taction, ::reflex_dispatcher::SchedulerTransition<#taction, #ttask>> for #actor_ident<#args> #where_clause {
            fn accept(&self, message: &#taction) -> bool {
                match self {
                    #(#accept_variants),*
                }
            }

            fn schedule(&self, message: &#taction, state: &Self::State) -> Option<::reflex_dispatcher::SchedulerMode> {
                match (self, state) {
                    #(#schedule_variants),*
                }
            }
        }

    }
}

fn impl_handler_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params,
        args,
        where_clause,
    } = &input.ttask_taction_generics;
    let actor_ident = &input.actor_ident;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let state_ident = &input.state_ident;
    let handle_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                (Self::#variant_name(actor), #state_ident::#variant_name(state)) => {
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Handler<#taction, ::reflex_dispatcher::SchedulerTransition<#taction,#ttask>>>::handle(actor, state, action, metadata,context)
                }
                }
        })
        .collect();

    quote_spanned! { span =>
        impl<#params> ::reflex_dispatcher::Handler<#taction, ::reflex_dispatcher::SchedulerTransition<#taction, #ttask>> for #actor_ident<#args> #where_clause {
            type State = #state_ident<#args>;
            fn handle(
                &self,
                state: &mut Self::State,
                action: &#taction,
                metadata: &::reflex_dispatcher::MessageData,
                context: &mut impl ::reflex_dispatcher::HandlerContext,
            ) -> Option<::reflex_dispatcher::SchedulerTransition<#taction, #ttask>> {
                match (self, state) {
                    #(#handle_variants),*
                }
            }
        }

    }
}

fn state_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let vis = &input.factory_vis;
    let state_ident = &input.state_ident;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let GenericsComponents {
        params,
        where_clause,
        ..
    } = &input.ttask_taction_generics;
    let state_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                #variant_name(
                    < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Handler<#taction, ::reflex_dispatcher::SchedulerTransition<#taction,#ttask>>>::State
                )
            }
        })
        .collect();
    quote_spanned! { span =>
        #vis enum #state_ident<#params> #where_clause {
            #(#state_variants),*
        }
    }
}

fn events_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let vis = &input.factory_vis;
    let events_ident = &input.events_ident;
    let project_ident = format_ident!("{}Variant", events_ident);
    let GenericsComponents {
        params,
        where_clause,
        ..
    } = &input.ttask_taction_generics;
    let taction = &input.taction;
    let ttask = &input.ttask;
    let events_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                #variant_name(
                    #[pin] < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Actor<#taction, #ttask> >::Events<TInbox>
                )
            }
        })
        .collect();
    let events_poll_next_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, _)| {
            quote_spanned! {*span =>
                     #project_ident::#variant_name(inner) => inner.poll_next(cx)
            }
        })
        .collect();
    let events_size_hint_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, _)| {
            quote_spanned! {*span =>
                     Self::#variant_name(inner) => inner.size_hint()
            }
        })
        .collect();
    quote_spanned! { span =>
        #[::pin_project::pin_project(project = #project_ident)]
        #vis enum #events_ident<#params, TInbox> #where_clause
              TInbox: ::reflex_dispatcher::TaskInbox<#taction>, {
            #(#events_variants),*
        }
        impl<#params, TInbox> ::futures::Stream for #events_ident<#params, TInbox> #where_clause
         TInbox: ::reflex_dispatcher::TaskInbox<#taction>, {
            type Item = TInbox::Message;
            fn poll_next(
                self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Option<Self::Item>> {
                match self.project() {
                    #(#events_poll_next_variants),*
                }
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    #(#events_size_hint_variants),*
                }
            }
        }
    }
}

fn named_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let GenericsComponents {
        params,
        where_clause,
        ..
    } = &input.ttask_taction_generics;
    let actor_ident = &input.actor_ident;
    let name_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, _)| {
            quote_spanned! {*span =>
                Self::#variant_name(inner) => inner.name()
            }
        })
        .collect();
    quote_spanned! { span =>
        impl<#params> ::reflex_dispatcher::Named for #actor_ident<#params> #where_clause {
            fn name(&self) -> &'static str {
                match self {
                    #(#name_variants),*
                }
            }
        }
    }
}

fn dispose_for_actor(input: &TaskFactoryConfiguration) -> TokenStream2 {
    let span = input.factory_ident.span();
    let vis = &input.factory_vis;
    let dispose_ident = &input.dispose_ident;
    let project_ident = format_ident!("{}Variant", dispose_ident);
    let taction = &input.taction;
    let ttask = &input.ttask;
    let GenericsComponents {
        params,
        where_clause,
        ..
    } = &input.ttask_taction_generics;
    let dispose_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, field)| {
            quote_spanned! {*span =>
                #variant_name(
                    #[pin] < < #field as ::reflex_dispatcher::TaskFactory<#taction, #ttask> >::Actor as ::reflex_dispatcher::Actor<#taction, #ttask> >::Dispose
                )
            }
        })
        .collect();
    let dispose_project_variants: Vec<TokenStream2> = input
        .factory_variants
        .parsed_variants
        .iter()
        .map(|(span, variant_name, _)| {
            quote_spanned! {*span =>
                     #project_ident::#variant_name(inner) => inner.poll(cx)
            }
        })
        .collect();
    quote_spanned! { span =>
        #[::pin_project::pin_project(project = #project_ident)]
        #vis enum #dispose_ident<#params> #where_clause {
            #(#dispose_variants),*
        }
        impl<#params> ::futures::Future for #dispose_ident<#params> #where_clause {
            type Output = ();
            fn poll(
                self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Self::Output> {
                match self.project() {
                    #(#dispose_project_variants),*
                }
            }
        }

    }
}

fn parse_taction_ttask(impl_template: &mut ItemImpl) -> syn::Result<(Type, Type)> {
    match impl_template.trait_.take().map(|(_, path, _)| path) {
        Some(path) => match &path.leading_colon {
            Some(_) => Err(path.span()),
            None => {
                let mut path_segments = path.segments.iter();
                match (path_segments.next(), path_segments.next()) {
                    (Some(segment), None) => match &segment.ident == TASK_FACTORY_TRAIT_NAME {
                        true => match &segment.arguments {
                            PathArguments::AngleBracketed(generic_args) => {
                                let mut args = generic_args.args.iter();
                                match (args.next(), args.next(), args.next()) {
                                    (Some(taction), Some(ttask), None) => match (taction, ttask) {
                                        (
                                            GenericArgument::Type(taction),
                                            GenericArgument::Type(ttask),
                                        ) => Ok((taction.clone(), ttask.clone())),
                                        _ => Err(generic_args.span()),
                                    },
                                    _ => Err(generic_args.span()),
                                }
                            }
                            _ => Err(segment.arguments.span()),
                        },
                        false => Err(segment.span()),
                    },
                    _ => Err(path.span()),
                }
            }
        }
        .map_err(|span| {
            syn::Error::new(
                span,
                format!("Invalid {} trait definition", TASK_FACTORY_TRAIT_NAME),
            )
        }),
        None => Err(syn::Error::new(
            impl_template.impl_token.span(),
            format!("Missing {} trait definition", TASK_FACTORY_TRAIT_NAME),
        )),
    }
}
