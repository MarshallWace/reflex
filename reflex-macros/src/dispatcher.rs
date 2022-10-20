// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use proc_macro::TokenStream;
use quote::{ToTokens, __private::Span, quote_spanned};
use syn::{
    punctuated::Punctuated, spanned::Spanned, token, AngleBracketedGenericArguments, AttrStyle,
    Attribute, Block, Error, Expr, ExprPath, Field, Fields, FieldsNamed, FnArg, GenericArgument,
    GenericParam, Generics, Ident, ImplItem, ImplItemMethod, ImplItemType, Item, ItemEnum,
    ItemImpl, ItemStruct, ItemTrait, Lifetime, LifetimeDef, PatType, Path, PathArguments,
    PathSegment, PredicateType, Result, Signature, Stmt, TraitBound, TraitBoundModifier, Type,
    TypeParam, TypeParamBound, TypePath, TypeReference, Variant, Visibility, WhereClause,
    WherePredicate,
};

const TYPE_NAME_STATE: &'static str = "State";
const TYPE_NAME_EVENTS: &'static str = "Events";
const TYPE_NAME_DISPOSE: &'static str = "Dispose";
const METHOD_NAME_INIT: &'static str = "init";
const METHOD_NAME_ACCEPT: &'static str = "accept";
const METHOD_NAME_SCHEDULE: &'static str = "schedule";
const METHOD_NAME_HANDLE: &'static str = "handle";

const IMPORT_CRATE: &'static str = "reflex_dispatcher";
const DISPATCHER_TRAIT_NAME: &'static str = "Dispatcher";
const ACTOR_TRAIT_NAME: &'static str = "Actor";
const HANDLER_TRAIT_NAME: &'static str = "Handler";
const WORKER_TRAIT_NAME: &'static str = "Worker";
const MATCHER_TRAIT_NAME: &'static str = "Matcher";
const IMPORT_PATH_ACTOR: [&'static str; 2] = [IMPORT_CRATE, ACTOR_TRAIT_NAME];
const IMPORT_PATH_HANDLER: [&'static str; 2] = [IMPORT_CRATE, HANDLER_TRAIT_NAME];
const IMPORT_PATH_WORKER: [&'static str; 2] = [IMPORT_CRATE, WORKER_TRAIT_NAME];
const IMPORT_PATH_MATCHER: [&'static str; 2] = [IMPORT_CRATE, MATCHER_TRAIT_NAME];
const IMPORT_PATH_HANDLER_OUTPUT: [&'static str; 2] = [IMPORT_CRATE, "SchedulerTransition"];

const ACTION_TYPE_INBOX: &'static str = "Inbox";
const ACTION_TYPE_OUTBOX: &'static str = "Outbox";

pub fn execute(input: TokenStream) -> TokenStream {
    syn::parse(input)
        .map_err(|err| Error::new(err.span(), "Expected block"))
        .and_then(|body| {
            let options = parse_dispatcher_macro_options(body)?;
            create_actor_impl(options)
        })
        .unwrap_or_else(|err| err.to_compile_error().into())
}

#[derive(Clone)]
struct DispatcherMacroOptions {
    actor_type_path: TypePath,
    handler_type_name: Ident,
    action_types: DispatcherActionTypes,
    type_params: DispatcherTypeParameters,
    action_trait: ItemTrait,
    state_type: ImplItemType,
    events_type: ImplItemType,
    dispose_type: ImplItemType,
    init_method: ImplItemMethod,
    actor_impl_template: ItemImpl,
    handler_lifetime: Lifetime,
    handler_impl_template: ItemImpl,
    handler_struct: ItemStruct,
    handler_methods: Vec<HandlerMethod>,
}

#[derive(Default, Clone, Debug)]
struct DispatcherActionTypes {
    inbox: Vec<Type>,
    outbox: Vec<Type>,
}

#[derive(Clone)]
struct DispatcherTypeParameters {
    message_type: Type,
    task_type: Type,
    action_trait_bounds: [WherePredicate; 1],
    span: Span,
}

#[derive(Clone)]
struct HandlerMethod {
    input_type: Type,
    accept_method: ImplItemMethod,
    schedule_method: ImplItemMethod,
    handle_method: ImplItemMethod,
}

fn parse_dispatcher_macro_options(body: Block) -> Result<DispatcherMacroOptions> {
    let span = body.span();
    let Block { stmts, .. } = body;
    let mut block_stmts = stmts.into_iter();
    let actions_enum = parse_item_enum(&mut block_stmts, span)?;
    let (action_types, action_trait) = parse_action_trait(actions_enum)?;
    let mut impl_template = parse_item_impl(&mut block_stmts, span)?;
    let actor_type_path = parse_actor_type_path(&impl_template)?;
    let actor_type_name = parse_actor_type_name(&actor_type_path)?;
    let handler_type_name = get_handler_type_name(&actor_type_name);
    let (message_type, task_type, generic_span) =
        parse_dispatcher_trait_parameters(&mut impl_template)?;
    let type_params = {
        let action_trait_path = create_ident_path(
            None,
            [action_trait.ident.clone()],
            action_trait
                .generics
                .params
                .iter()
                .map(convert_generic_param_to_argument),
        );
        let action_trait_bounds =
            create_action_trait_bounds(action_trait_path.clone(), message_type.clone());
        DispatcherTypeParameters {
            message_type,
            task_type,
            action_trait_bounds,
            span: generic_span,
        }
    };
    if let Some(trailing) = block_stmts.next() {
        return Err(Error::new(trailing.span(), "Unexpected statement"));
    }
    let span = impl_template.span();
    let items = std::mem::take(&mut impl_template.items);
    let mut impl_items = items.into_iter();
    let state_type = parse_impl_state_type(&mut impl_items, span)?;
    let events_type = parse_impl_events_type(&mut impl_items, span)?;
    let dispose_type = parse_impl_dispose_type(&mut impl_items, span)?;
    let init_method = parse_impl_init_method(&mut impl_items, span)?;
    let handler_methods = parse_impl_handler_methods(&mut impl_items, span)?;
    let actor_impl_template = impl_template.clone();
    let handler_lifetime = Lifetime::new("'_a", impl_template.impl_token.span());
    let handler_impl_template = {
        let mut handler_impl_template = impl_template;
        match handler_impl_template.self_ty.as_mut() {
            Type::Path(type_path) => match type_path.path.segments.last_mut() {
                Some(segment) => {
                    segment.ident = handler_type_name.clone();
                    Ok(())
                }
                _ => Err(type_path.span()),
            },
            self_ty => Err(self_ty.span()),
        }
        .map_err(|span| Error::new(span, "Invalid type name"))?;
        prepend_lifetime_generic_param(
            &mut handler_impl_template.generics,
            handler_lifetime.clone(),
        );
        modify_handler_method_generic_args(
            &mut handler_impl_template.self_ty,
            handler_lifetime.clone(),
            &type_params,
        )?;
        handler_impl_template
    };
    let handler_struct = {
        let span = handler_impl_template.impl_token.span();
        ItemStruct {
            attrs: Default::default(),
            vis: Visibility::Inherited,
            struct_token: Default::default(),
            ident: handler_type_name.clone(),
            generics: handler_impl_template.generics.clone(),
            fields: Fields::Named(FieldsNamed {
                brace_token: Default::default(),
                named: [
                    Field {
                        attrs: Default::default(),
                        vis: Visibility::Inherited,
                        ident: Some(Ident::new("inner", span)),
                        colon_token: None,
                        ty: Type::Reference(TypeReference {
                            and_token: Default::default(),
                            lifetime: Some(handler_lifetime.clone()),
                            mutability: None,
                            elem: Box::new(Type::Path(actor_type_path.clone())),
                        }),
                    },
                    Field {
                        attrs: Default::default(),
                        vis: Visibility::Inherited,
                        ident: Some(Ident::new("_message", span)),
                        colon_token: None,
                        ty: Type::Path(TypePath {
                            qself: None,
                            path: create_import_path(
                                ["std", "marker", "PhantomData"],
                                [GenericArgument::Type(type_params.message_type.clone())],
                                span,
                            ),
                        }),
                    },
                    Field {
                        attrs: Default::default(),
                        vis: Visibility::Inherited,
                        ident: Some(Ident::new("_task", span)),
                        colon_token: None,
                        ty: Type::Path(TypePath {
                            qself: None,
                            path: create_import_path(
                                ["std", "marker", "PhantomData"],
                                [GenericArgument::Type(type_params.task_type.clone())],
                                span,
                            ),
                        }),
                    },
                ]
                .into_iter()
                .collect(),
            }),
            semi_token: Some(Default::default()),
        }
    };
    if let Some(trailing) = impl_items.next() {
        return Err(Error::new(trailing.span(), "Unexpected item"));
    }
    Ok(DispatcherMacroOptions {
        actor_type_path,
        handler_type_name,
        action_types,
        type_params,
        action_trait,
        state_type,
        events_type,
        dispose_type,
        init_method,
        actor_impl_template,
        handler_lifetime,
        handler_impl_template,
        handler_struct,
        handler_methods,
    })
}

fn get_handler_type_name(base_type_name: &Ident) -> Ident {
    Ident::new(
        &format!("__{}Handler", base_type_name),
        base_type_name.span(),
    )
}

fn create_actor_impl(options: DispatcherMacroOptions) -> Result<TokenStream> {
    let DispatcherMacroOptions {
        actor_type_path,
        handler_type_name,
        action_types,
        actor_impl_template,
        handler_lifetime,
        handler_impl_template,
        type_params,
        action_trait,
        state_type,
        events_type,
        dispose_type,
        handler_struct,
        init_method,
        handler_methods,
    } = options;
    let action_trait = create_action_trait_impl(action_trait)?;
    let actor_impl = create_actor_init_impl(
        events_type,
        dispose_type,
        init_method,
        &actor_impl_template,
        &type_params,
    );
    let dispatch_method_impl = create_actor_dispatch_impl(
        handler_type_name,
        &action_types.inbox,
        state_type.clone(),
        &actor_impl_template,
        &type_params,
    )?;
    let handler_impl = create_handler_impl(
        handler_struct,
        handler_lifetime,
        actor_type_path,
        &handler_impl_template,
        &type_params,
    )?;
    let (worker_methods, handler_methods): (Vec<_>, Vec<_>) = handler_methods
        .into_iter()
        .map(|method| {
            let HandlerMethod {
                input_type,
                accept_method,
                schedule_method,
                handle_method,
            } = method;
            let worker_methods = (input_type.clone(), accept_method, schedule_method);
            let handler_methods = (input_type.clone(), handle_method);
            (worker_methods, handler_methods)
        })
        .unzip();
    let worker_impls =
        create_handler_worker_impls(worker_methods, &handler_impl_template, &type_params);
    let handler_impls = create_handler_method_impls(
        state_type,
        handler_methods,
        &handler_impl_template,
        &type_params,
    );
    Ok(TokenStream::from_iter([
        action_trait,
        actor_impl,
        dispatch_method_impl,
        handler_impl,
        worker_impls,
        handler_impls,
    ]))
}

fn create_action_trait_impl(trait_definition: ItemTrait) -> Result<TokenStream> {
    let span = trait_definition.span();
    let self_type_ident = Ident::new("_Self", span);
    let self_param = GenericParam::Type(TypeParam {
        attrs: Default::default(),
        ident: self_type_ident.clone(),
        colon_token: None,
        bounds: Default::default(),
        eq_token: None,
        default: None,
    });
    let trait_impl_generics = Generics {
        lt_token: Default::default(),
        params: [self_param]
            .into_iter()
            .chain(trait_definition.generics.params.iter().cloned())
            .collect(),
        gt_token: Default::default(),
        where_clause: trait_definition.generics.where_clause.clone(),
    };
    let trait_impl_path = create_ident_path(
        None,
        [trait_definition.ident.clone()],
        trait_definition
            .generics
            .params
            .iter()
            .map(convert_generic_param_to_argument),
    );
    let trait_bounds = &trait_definition
        .supertraits
        .iter()
        .cloned()
        .collect::<Vec<_>>();
    let output = quote_spanned! {span=>
        #trait_definition
        impl #trait_impl_generics #trait_impl_path for _Self where Self: #(#trait_bounds)+* {}
    }
    .into();
    Ok(output)
}

fn create_actor_init_impl(
    events_type: ImplItemType,
    dispose_type: ImplItemType,
    init_method: ImplItemMethod,
    template: &ItemImpl,
    type_params: &DispatcherTypeParameters,
) -> TokenStream {
    let input_type = type_params.message_type.clone();
    let output_type = type_params.task_type.clone();
    let trait_path = create_import_path(
        IMPORT_PATH_ACTOR,
        [
            GenericArgument::Type(input_type),
            GenericArgument::Type(output_type),
        ],
        template.impl_token.span(),
    );
    let template = {
        let mut template = template.clone();
        let span = init_method.sig.ident.span();
        extend_generic_constraints(&mut template.generics, get_action_trait_bounds(type_params));
        append_automatically_derived_annotation(&mut template, span);
        template
    };
    create_trait_impl(
        template,
        trait_path,
        [
            ImplItem::Type(events_type),
            ImplItem::Type(dispose_type),
            ImplItem::Method(init_method),
        ],
    )
}

fn create_actor_dispatch_impl(
    handler_type_name: Ident,
    input_types: &[Type],
    state_type: ImplItemType,
    template: &ItemImpl,
    type_params: &DispatcherTypeParameters,
) -> Result<TokenStream> {
    let span = template.impl_token.span();
    let input_type = type_params.message_type.clone();
    let output_type = create_handler_output_type(
        type_params.message_type.clone(),
        type_params.task_type.clone(),
        type_params.span.clone(),
    );
    let message_type = &type_params.message_type;
    let task_type = &type_params.task_type;
    let (accept_method, schedule_method, handle_method) = syn::parse(TokenStream::from(quote_spanned!(span=> {
        impl Dispatcher {
            fn accept(&self, message: &#input_type) -> bool {
                None
                    #(.or_else(||
                        <#input_type as ::reflex_dispatcher::Matcher<#input_types>>::match_type(message)
                            .map(|message|
                                #handler_type_name::new(
                                    self,
                                    ::std::marker::PhantomData::<#message_type>,
                                    ::std::marker::PhantomData::<#task_type>
                                ).accept(message)
                            )
                    ))*
                    .unwrap_or(false)
            }
            fn schedule(&self, message: &#input_type, state: &Self::State) -> ::std::option::Option<::reflex_dispatcher::SchedulerMode> {
                None
                    #(.or_else(||
                        <#input_type as ::reflex_dispatcher::Matcher<#input_types>>::match_type(message)
                            .map(|message|
                                #handler_type_name::new(
                                    self,
                                    ::std::marker::PhantomData::<#message_type>,
                                    ::std::marker::PhantomData::<#task_type>
                                ).schedule(message, state)
                            )
                    ))*
                    .and_then(|result| result)
            }
            fn handle(
                &self,
                state: &mut Self::State,
                message: &#input_type,
                metadata: &::reflex_dispatcher::MessageData,
                context: &mut impl ::reflex_dispatcher::HandlerContext,
            ) -> ::std::option::Option<#output_type> {
                None
                    #(.or_else(||
                        <#input_type as ::reflex_dispatcher::Matcher<#input_types>>::match_type(message)
                            .map(|message|
                                #handler_type_name::new(
                                    self,
                                    ::std::marker::PhantomData::<#message_type>,
                                    ::std::marker::PhantomData::<#task_type>
                                ).handle(state, message, metadata, context)
                            )
                    ))*
                    .and_then(|result| result)
            }
        }
    })))
    .and_then(|ast: Block| {
        match ast.stmts.into_iter().next() {
            Some(Stmt::Item(Item::Impl(ast))) => {
                let mut items = ast.items.into_iter();
                match (items.next(), items.next(), items.next()) {
                (
                    Some(ImplItem::Method(accept_method)),
                    Some(ImplItem::Method(schedule_method)),
                    Some(ImplItem::Method(handle_method)),
                 ) => Some((accept_method, schedule_method, handle_method)),
                _ => None,
            }},
            _ => None,
        }
        .ok_or_else(|| {
            Error::new(
                Span::call_site(),
                "Failed to generate method implementation",
            )
        })
    })?;
    let template = {
        let mut template = template.clone();
        extend_generic_constraints(&mut template.generics, get_action_trait_bounds(type_params));
        append_automatically_derived_annotation(&mut template, span);
        template
    };
    let worker_impl = create_trait_impl(
        template.clone(),
        create_import_path(
            IMPORT_PATH_WORKER,
            [
                GenericArgument::Type(input_type.clone()),
                GenericArgument::Type(output_type.clone()),
            ],
            span,
        ),
        [
            ImplItem::Method(accept_method),
            ImplItem::Method(schedule_method),
        ],
    );
    let handler_impl = create_trait_impl(
        template,
        create_import_path(
            IMPORT_PATH_HANDLER,
            [
                GenericArgument::Type(input_type.clone()),
                GenericArgument::Type(output_type.clone()),
            ],
            span,
        ),
        [ImplItem::Type(state_type), ImplItem::Method(handle_method)],
    );
    Ok(TokenStream::from_iter([worker_impl, handler_impl]))
}

fn extend_generic_constraints(
    generics: &mut Generics,
    predicates: impl IntoIterator<Item = WherePredicate>,
) {
    let where_clause = &mut generics.where_clause;
    let mut combined_where_clause = where_clause.take().unwrap_or_else(|| WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });
    combined_where_clause.predicates.extend(predicates);
    where_clause.replace(combined_where_clause);
}

fn append_automatically_derived_annotation(item: &mut ItemImpl, span: Span) {
    item.attrs.push(Attribute {
        pound_token: Default::default(),
        style: AttrStyle::Outer,
        bracket_token: Default::default(),
        path: create_ident_path(None, [Ident::new("automatically_derived", span)], []),
        tokens: Default::default(),
    });
}

fn create_handler_impl(
    handler_struct: ItemStruct,
    handler_lifetime: Lifetime,
    actor_type_path: TypePath,
    template: &ItemImpl,
    type_params: &DispatcherTypeParameters,
) -> Result<TokenStream> {
    let generic_params = &handler_struct.generics.params;
    let where_clause = &handler_struct.generics.where_clause;
    let self_type = &template.self_ty;
    let target_type = actor_type_path;
    let target_type_ref = Type::Reference(TypeReference {
        and_token: Default::default(),
        lifetime: Some(handler_lifetime),
        mutability: None,
        elem: Box::new(Type::Path(target_type.clone())),
    });
    let message_type = &type_params.message_type;
    let task_type = &type_params.task_type;
    let span = handler_struct.ident.span();
    let deref_impl = TokenStream::from(quote_spanned! {span=>
        impl<#generic_params> #self_type #where_clause {
            fn new(
                inner: #target_type_ref,
                message: ::std::marker::PhantomData<#message_type>,
                task: ::std::marker::PhantomData<#task_type>,
            ) -> Self {
                Self { inner, _message: message, _task: task }
            }
        }
        impl<#generic_params> ::std::ops::Deref for #self_type #where_clause {
            type Target = #target_type;
            fn deref(&self) -> &Self::Target {
                self.inner
            }
        }
    });
    let handler_struct = TokenStream::from(handler_struct.into_token_stream());
    Ok(TokenStream::from_iter([handler_struct, deref_impl]))
}

fn create_handler_worker_impls(
    worker_methods: impl IntoIterator<Item = (Type, ImplItemMethod, ImplItemMethod)>,
    template: &ItemImpl,
    type_params: &DispatcherTypeParameters,
) -> TokenStream {
    let output_type = create_handler_output_type(
        type_params.message_type.clone(),
        type_params.task_type.clone(),
        type_params.span.clone(),
    );
    TokenStream::from_iter(worker_methods.into_iter().map(
        |(input_type, accept_method, schedule_method)| {
            let span = template.impl_token.span();
            let trait_path = create_import_path(
                IMPORT_PATH_WORKER,
                [
                    GenericArgument::Type(input_type.clone()),
                    GenericArgument::Type(output_type.clone()),
                ],
                span,
            );
            let template = {
                let mut template = template.clone();
                extend_generic_constraints(
                    &mut template.generics,
                    get_action_trait_bounds(type_params),
                );
                template
            };
            create_trait_impl(
                template,
                trait_path.clone(),
                [
                    ImplItem::Method(accept_method),
                    ImplItem::Method(schedule_method),
                ],
            )
        },
    ))
}

fn create_handler_method_impls(
    state_type: ImplItemType,
    handler_methods: impl IntoIterator<Item = (Type, ImplItemMethod)>,
    template: &ItemImpl,
    type_params: &DispatcherTypeParameters,
) -> TokenStream {
    let output_type = create_handler_output_type(
        type_params.message_type.clone(),
        type_params.task_type.clone(),
        type_params.span.clone(),
    );
    TokenStream::from_iter(
        handler_methods
            .into_iter()
            .map(|(input_type, handler_method)| {
                let span = template.impl_token.span();
                let trait_path = create_import_path(
                    IMPORT_PATH_HANDLER,
                    [
                        GenericArgument::Type(input_type.clone()),
                        GenericArgument::Type(output_type.clone()),
                    ],
                    span,
                );
                let template = {
                    let mut template = template.clone();
                    extend_generic_constraints(
                        &mut template.generics,
                        get_action_trait_bounds(type_params),
                    );
                    template
                };
                create_trait_impl(
                    template.clone(),
                    trait_path.clone(),
                    [
                        ImplItem::Type(state_type.clone()),
                        ImplItem::Method(handler_method),
                    ],
                )
            }),
    )
}

fn create_handler_output_type(message_type: Type, task_type: Type, span: Span) -> Type {
    Type::Path(TypePath {
        qself: None,
        path: create_import_path(
            IMPORT_PATH_HANDLER_OUTPUT,
            [
                GenericArgument::Type(message_type.clone()),
                GenericArgument::Type(task_type.clone()),
            ],
            span,
        ),
    })
}

fn parse_action_trait(actions_enum: ItemEnum) -> Result<(DispatcherActionTypes, ItemTrait)> {
    let span = actions_enum.span();
    let ItemEnum {
        vis,
        ident,
        generics,
        variants,
        ..
    } = actions_enum;
    let action_types = variants.into_iter().fold(
        Result::<DispatcherActionTypes>::Ok(Default::default()),
        |result, variant| {
            let mut action_types = result?;
            let target_list = if &variant.ident == ACTION_TYPE_INBOX {
                Some(&mut action_types.inbox)
            } else if &variant.ident == ACTION_TYPE_OUTBOX {
                Some(&mut action_types.outbox)
            } else {
                None
            }
            .ok_or_else(|| {
                Error::new(
                    variant.ident.span(),
                    "Expected Inbox or Outbox message type",
                )
            })?;
            let action_type = parse_action_type_variant(&variant)?;
            target_list.push(action_type.clone());
            Ok(action_types)
        },
    )?;
    let action_trait = ItemTrait {
        attrs: Default::default(),
        vis,
        unsafety: None,
        auto_token: None,
        trait_token: Default::default(),
        ident,
        generics,
        colon_token: Some(Default::default()),
        supertraits: action_types
            .inbox
            .iter()
            .map(|input_type| {
                TypeParamBound::Trait(TraitBound {
                    paren_token: Default::default(),
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: create_import_path(
                        IMPORT_PATH_MATCHER,
                        [GenericArgument::Type(input_type.clone())],
                        span,
                    ),
                })
            })
            .chain(action_types.outbox.iter().map(|output_type| {
                TypeParamBound::Trait(TraitBound {
                    paren_token: Default::default(),
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: create_import_path(
                        ["std", "convert", "From"],
                        [GenericArgument::Type(output_type.clone())],
                        span,
                    ),
                })
            }))
            .collect(),
        brace_token: Default::default(),
        items: Default::default(),
    };
    Ok((action_types, action_trait))
}

fn parse_action_type_variant(variant: &Variant) -> Result<&Type> {
    match &variant.fields {
        Fields::Unnamed(fields) => {
            let mut fields = fields.unnamed.iter();
            match (fields.next(), fields.next()) {
                (Some(field), None) => Ok(&field.ty),
                (Some(_), Some(field)) => Err(Error::new(field.span(), "Unexpected field")),
                _ => Err(Error::new(variant.span(), "Missing message type")),
            }
        }
        _ => Err(Error::new(
            variant.span(),
            "Invalid message type definition",
        )),
    }
}

fn parse_dispatcher_trait_parameters(impl_template: &mut ItemImpl) -> Result<(Type, Type, Span)> {
    match impl_template.trait_.take().map(|(_colon, path, _for)| path) {
        Some(path) => match &path.leading_colon {
            Some(_) => Err(path.span()),
            None => {
                let mut path_segments = path.segments.iter();
                match (path_segments.next(), path_segments.next()) {
                    (Some(segment), None) => match &segment.ident == DISPATCHER_TRAIT_NAME {
                        true => match &segment.arguments {
                            PathArguments::AngleBracketed(generic_args) => {
                                let mut args = generic_args.args.iter();
                                match (args.next(), args.next(), args.next()) {
                                    (Some(message_type), Some(task_type), None) => {
                                        match (message_type, task_type) {
                                            (
                                                GenericArgument::Type(message_type),
                                                GenericArgument::Type(task_type),
                                            ) => Ok((
                                                message_type.clone(),
                                                task_type.clone(),
                                                generic_args.span(),
                                            )),
                                            _ => Err(generic_args.span()),
                                        }
                                    }
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
            Error::new(
                span,
                format!("Invalid {} trait definition", DISPATCHER_TRAIT_NAME),
            )
        }),
        None => Err(Error::new(
            impl_template.impl_token.span(),
            format!("Missing {} trait definition", DISPATCHER_TRAIT_NAME),
        )),
    }
}

fn create_action_trait_bounds(action_trait_path: Path, message_type: Type) -> [WherePredicate; 1] {
    [WherePredicate::Type(PredicateType {
        lifetimes: None,
        bounded_ty: message_type,
        colon_token: Default::default(),
        bounds: [TypeParamBound::Trait(TraitBound {
            paren_token: Default::default(),
            modifier: TraitBoundModifier::None,
            lifetimes: None,
            path: action_trait_path,
        })]
        .into_iter()
        .collect(),
    })]
}

fn get_action_trait_bounds(
    type_params: &DispatcherTypeParameters,
) -> impl Iterator<Item = WherePredicate> + '_ {
    type_params.action_trait_bounds.iter().cloned()
}

fn parse_actor_type_path(impl_template: &ItemImpl) -> Result<TypePath> {
    match impl_template.self_ty.as_ref() {
        Type::Path(self_ty) => Ok(self_ty.clone()),
        self_ty => Err(self_ty.span()),
    }
    .map_err(|span| Error::new(span, "Invalid Actor base type"))
}

fn parse_actor_type_name(type_path: &TypePath) -> Result<Ident> {
    type_path
        .path
        .segments
        .last()
        .map(|segment| &segment.ident)
        .cloned()
        .ok_or_else(|| Error::new(type_path.span(), "Invalid Actor base type name"))
}

fn parse_impl_state_type(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemType> {
    parse_named_impl_item_type(TYPE_NAME_STATE, items, span)
}

fn parse_impl_events_type(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemType> {
    parse_named_impl_item_type(TYPE_NAME_EVENTS, items, span)
}

fn parse_impl_dispose_type(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemType> {
    parse_named_impl_item_type(TYPE_NAME_DISPOSE, items, span)
}

fn parse_impl_init_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    parse_named_impl_method(METHOD_NAME_INIT, items, span)
}

fn parse_impl_handler_methods(
    items: &mut (impl Iterator<Item = ImplItem> + ExactSizeIterator),
    span: Span,
) -> Result<Vec<HandlerMethod>> {
    let mut methods = Vec::new();
    while items.len() > 0 {
        let method = parse_impl_handler_method(items, span)?;
        methods.push(method);
    }
    Ok(methods)
}

fn parse_impl_handler_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<HandlerMethod> {
    let accept_method = parse_impl_handler_accept_method(items, span)?;
    let accept_input_type = parse_accept_method_type(&accept_method).cloned()?;
    let schedule_method = parse_impl_handler_schedule_method(items, span)?;
    let schedule_input_type = parse_schedule_method_type(&schedule_method)?;
    if schedule_input_type != &accept_input_type {
        return Err(Error::new(
            schedule_input_type.span(),
            format!("Expected: {}", accept_input_type.into_token_stream()),
        ));
    }
    let handle_method = parse_impl_handler_handle_method(items, span)?;
    let handle_input_type = parse_handle_method_type(&handle_method)?;
    if handle_input_type != &accept_input_type {
        return Err(Error::new(
            handle_input_type.span(),
            format!("Expected: {}", accept_input_type.into_token_stream()),
        ));
    }
    let input_type = parse_reference_type_target(accept_input_type)?;
    Ok(HandlerMethod {
        input_type,
        accept_method,
        schedule_method,
        handle_method,
    })
}

fn parse_reference_type_target(ref_type: Type) -> Result<Type> {
    match ref_type {
        Type::Reference(ref_type) => Ok(*ref_type.elem),
        _ => Err(Error::new(ref_type.span(), "Expected type reference")),
    }
}

fn parse_impl_handler_accept_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    parse_named_impl_method(METHOD_NAME_ACCEPT, items, span)
}

fn parse_impl_handler_schedule_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    parse_named_impl_method(METHOD_NAME_SCHEDULE, items, span)
}

fn parse_impl_handler_handle_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    parse_named_impl_method(METHOD_NAME_HANDLE, items, span)
}

fn parse_accept_method_type(method: &ImplItemMethod) -> Result<&Type> {
    let span = method.sig.inputs.span();
    let _ = parse_self_ref_method_receiver(&method.sig)?;
    let mut args = method.sig.inputs.iter().skip(1);
    let arg = parse_method_arg(&mut args, span)?;
    Ok(&arg.ty)
}

fn parse_schedule_method_type(method: &ImplItemMethod) -> Result<&Type> {
    let span = method.sig.inputs.span();
    let _ = parse_self_ref_method_receiver(&method.sig)?;
    let mut args = method.sig.inputs.iter().skip(1);
    let arg = parse_method_arg(&mut args, span)?;
    Ok(&arg.ty)
}

fn parse_handle_method_type(method: &ImplItemMethod) -> Result<&Type> {
    let span = method.sig.inputs.span();
    let _ = parse_self_ref_method_receiver(&method.sig)?;
    let mut args = method.sig.inputs.iter().skip(1);
    let _state = parse_method_arg(&mut args, span)?;
    let arg = parse_method_arg(&mut args, span)?;
    Ok(&arg.ty)
}

fn prepend_lifetime_generic_param(generics: &mut Generics, lifetime: Lifetime) {
    generics.params.insert(
        0,
        GenericParam::Lifetime(LifetimeDef {
            attrs: Default::default(),
            lifetime,
            colon_token: None,
            bounds: Default::default(),
        }),
    );
}

fn modify_handler_method_generic_args(
    target_type: &mut Type,
    lifetime: Lifetime,
    type_params: &DispatcherTypeParameters,
) -> Result<()> {
    mutate_generic_args(target_type, |generic_args| {
        generic_args
            .args
            .insert(0, GenericArgument::Lifetime(lifetime));
        generic_args
            .args
            .push(GenericArgument::Type(type_params.message_type.clone()));
        generic_args
            .args
            .push(GenericArgument::Type(type_params.task_type.clone()));
    })
}

fn mutate_generic_args(
    target_type: &mut Type,
    mutate: impl FnOnce(&mut AngleBracketedGenericArguments) -> (),
) -> Result<()> {
    let span = target_type.span();
    let (type_args, mut generic_args) = match target_type {
        Type::Path(TypePath { qself: None, path }) => match path.segments.last_mut() {
            None => None,
            Some(segment) => match std::mem::replace(&mut segment.arguments, PathArguments::None) {
                PathArguments::None => Some((
                    &mut segment.arguments,
                    AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Default::default(),
                        args: Default::default(),
                        gt_token: Default::default(),
                    },
                )),
                PathArguments::AngleBracketed(args) => Some((&mut segment.arguments, args)),
                PathArguments::Parenthesized(_) => None,
            },
        },
        _ => None,
    }
    .ok_or_else(|| Error::new(span, "Invalid type"))?;
    mutate(&mut generic_args);
    *type_args = PathArguments::AngleBracketed(generic_args);
    Ok(())
}

fn parse_item_enum(stmts: &mut impl Iterator<Item = Stmt>, span: Span) -> Result<ItemEnum> {
    match stmts.next() {
        Some(stmt) => match stmt {
            Stmt::Item(item) => match item {
                Item::Enum(item) => Ok(item),
                item => Err(item.span()),
            },
            stmt => Err(stmt.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected enum definition"))
}

fn parse_item_impl(stmts: &mut impl Iterator<Item = Stmt>, span: Span) -> Result<ItemImpl> {
    match stmts.next() {
        Some(stmt) => match stmt {
            Stmt::Item(item) => match item {
                Item::Impl(item) => Ok(item),
                item => Err(item.span()),
            },
            stmt => Err(stmt.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected impl block"))
}

fn parse_impl_item_type(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemType> {
    match items.next() {
        Some(item) => match item {
            ImplItem::Type(item) => Ok(item),
            item => Err(item.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected type declaration"))
}

fn parse_named_impl_item_type(
    name: &str,
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemType> {
    let type_definition = parse_impl_item_type(items, span)?;
    parse_named_term(name, type_definition, |type_definition| {
        &type_definition.ident
    })
}

fn parse_impl_method(
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    match items.next() {
        Some(item) => match item {
            ImplItem::Method(item) => Ok(item),
            item => Err(item.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected method implementation"))
}

fn parse_named_impl_method(
    name: &str,
    items: &mut impl Iterator<Item = ImplItem>,
    span: Span,
) -> Result<ImplItemMethod> {
    let method_definition = parse_impl_method(items, span)?;
    parse_named_term(name, method_definition, |method_definition| {
        &method_definition.sig.ident
    })
}

fn parse_self_ref_method_receiver(sig: &Signature) -> Result<()> {
    match sig.receiver() {
        Some(receiver) => match receiver {
            FnArg::Receiver(receiver) => match (&receiver.reference, &receiver.mutability) {
                (Some(_), None) => Some(()),
                _ => None,
            },
            _ => None,
        },
        None => None,
    }
    .ok_or_else(|| Error::new(sig.span(), "Expected: &self"))
}

fn parse_method_arg<'a>(
    args: &mut impl Iterator<Item = &'a FnArg>,
    span: Span,
) -> Result<&'a PatType> {
    match args.next() {
        Some(arg) => match arg {
            FnArg::Typed(arg) => Ok(arg),
            FnArg::Receiver(arg) => Err(arg.span()),
        },
        None => Err(span),
    }
    .map_err(|span| Error::new(span, "Expected argument"))
}

fn parse_named_term<T>(name: &str, value: T, name_selector: impl Fn(&T) -> &Ident) -> Result<T> {
    let ident = name_selector(&value);
    match ident == name {
        true => Ok(value),
        false => Err(Error::new(ident.span(), format!("Expected: \"{}\"", name))),
    }
}

fn convert_generic_param_to_argument(param: &GenericParam) -> GenericArgument {
    match param {
        GenericParam::Type(param) => GenericArgument::Type(Type::Path(TypePath {
            qself: None,
            path: create_ident_path(None, [param.ident.clone()], []),
        })),
        GenericParam::Lifetime(param) => GenericArgument::Lifetime(param.lifetime.clone()),
        GenericParam::Const(param) => GenericArgument::Const(Expr::Path(ExprPath {
            attrs: Default::default(),
            qself: None,
            path: create_ident_path(None, [param.ident.clone()], []),
        })),
    }
}

fn create_trait_impl(
    template: ItemImpl,
    trait_path: Path,
    members: impl IntoIterator<Item = ImplItem>,
) -> TokenStream {
    let mut implementation = template.clone();
    implementation.trait_ = Some((None, trait_path, Default::default()));
    implementation.items.extend(members);
    implementation.into_token_stream().into()
}

fn create_import_path<'a>(
    path_segments: impl IntoIterator<Item = &'a str>,
    generics: impl IntoIterator<Item = GenericArgument>,
    span: Span,
) -> Path {
    create_ident_path(
        Some(Default::default()),
        path_segments
            .into_iter()
            .map(|segment| Ident::new(segment, span)),
        generics,
    )
}

fn create_ident_path<'a>(
    absolute_prefix: Option<token::Colon2>,
    path_segments: impl IntoIterator<Item = Ident>,
    generics: impl IntoIterator<Item = GenericArgument>,
) -> Path {
    let generic_args = Some(generics.into_iter().collect::<Punctuated<_, _>>())
        .filter(|args| !args.is_empty())
        .map(|args| AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Default::default(),
            args,
            gt_token: Default::default(),
        });
    let path_segments = path_segments.into_iter();
    let mut segments = path_segments
        .map(|segment| PathSegment {
            ident: segment,
            arguments: PathArguments::None,
        })
        .collect::<Punctuated<_, _>>();
    if let Some((generic_args, Some(final_segment))) =
        generic_args.map(|args| (args, segments.last_mut()))
    {
        final_segment.arguments = PathArguments::AngleBracketed(generic_args)
    }
    Path {
        leading_colon: absolute_prefix,
        segments,
    }
}
