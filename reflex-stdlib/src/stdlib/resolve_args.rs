// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{Apply, CollectList, Sequence};

use reflex::core::{
    uuid, Applicable, ArgType, Arity, Builtin, BuiltinTermType, CompiledFunctionTermType,
    EvaluationCache, Expression, ExpressionFactory, ExpressionListType, FunctionArity,
    HeapAllocator, LambdaTermType, PartialApplicationTermType, RefType, Uid, Uuid,
};

pub struct ResolveArgs;
impl ResolveArgs {
    pub const UUID: Uuid = uuid!("3d67b92a-e64e-419b-a4a9-8e49bd1eae92");
    const ARITY: FunctionArity<1, 0> = FunctionArity {
        required: [ArgType::Strict],
        optional: [],
        variadic: None,
    };
    pub fn arity() -> Arity {
        Arity::from(&Self::ARITY)
    }
}
impl Uid for ResolveArgs {
    fn uid(&self) -> Uuid {
        Self::UUID
    }
}
impl<T: Expression> Applicable<T> for ResolveArgs
where
    T::Builtin: From<Apply> + From<CollectList> + From<Sequence>,
{
    fn arity(&self) -> Option<Arity> {
        Some(Self::arity())
    }
    fn should_parallelize(&self, _args: &[T]) -> bool {
        false
    }
    fn apply(
        &self,
        mut args: impl ExactSizeIterator<Item = T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        _cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let target = args.next().unwrap();
        if let Some(arity) = get_expression_arity(&target, factory)
            .filter(|arity| arity.optional().len() == 0 && arity.variadic().is_none())
        {
            let num_args = arity.required().len();
            let is_atomic_lambda = factory
                .match_lambda_term(&target)
                .map(|target| target.body().as_deref().is_atomic())
                .unwrap_or(false);
            if num_args == 0 || is_atomic_lambda {
                Ok(target)
            } else if num_args == 1 {
                Ok(factory.create_lambda_term(
                    1,
                    factory.create_application_term(
                        factory.create_builtin_term(Sequence),
                        allocator.create_pair(factory.create_variable_term(0), target),
                    ),
                ))
            } else {
                Ok(factory.create_lambda_term(
                    num_args,
                    factory.create_application_term(
                        factory.create_builtin_term(Apply),
                        allocator.create_pair(
                            target,
                            factory.create_application_term(
                                factory.create_builtin_term(CollectList),
                                allocator.create_sized_list(
                                    num_args,
                                    (0..num_args).map(|index| {
                                        factory.create_variable_term(num_args - 1 - index)
                                    }),
                                ),
                            ),
                        ),
                    ),
                ))
            }
        } else {
            Err(format!(
                "Expected non-variadic <function>, received {}",
                target
            ))
        }
    }
}

fn get_expression_arity<T: Expression>(
    target: &T,
    factory: &impl ExpressionFactory<T>,
) -> Option<Arity> {
    if let Some(target) = factory.match_lambda_term(target) {
        Some(Arity::lazy(target.num_args(), 0, false))
    } else if let Some(target) = factory.match_builtin_term(target) {
        Some(target.target().arity())
    } else if let Some(target) = factory.match_partial_application_term(target) {
        get_expression_arity(target.target().as_deref(), factory)
            .map(|arity| arity.partial(target.args().as_deref().len()))
    } else if let Some(target) = factory.match_compiled_function_term(target) {
        Some(Arity::lazy(
            target.required_args(),
            target.optional_args(),
            false,
        ))
    } else {
        None
    }
}
