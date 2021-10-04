// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{collections::HashSet, iter::once};

use crate::{
    cache::NoopCache,
    compiler::{Compile, Compiler, Instruction, NativeFunctionRegistry, Program},
    core::{
        Applicable, Arity, DependencyList, DynamicState, EvaluationCache, Expression,
        ExpressionFactory, GraphNode, HeapAllocator, Reducible, Rewritable, ScopeOffset,
        StackOffset, Substitutions, VarArgs,
    },
    hash::hash_object,
};

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct LambdaTerm<T: Expression> {
    num_args: StackOffset,
    body: T,
}
impl<T: Expression> LambdaTerm<T> {
    pub fn new(num_args: StackOffset, body: T) -> Self {
        Self { num_args, body }
    }
    pub fn num_args(&self) -> StackOffset {
        self.num_args
    }
    pub fn body(&self) -> &T {
        &self.body
    }
}
impl<T: Expression> GraphNode for LambdaTerm<T> {
    fn capture_depth(&self) -> StackOffset {
        self.body.capture_depth().saturating_sub(self.num_args)
    }
    fn free_variables(&self) -> HashSet<StackOffset> {
        let num_args = self.num_args;
        self.body
            .free_variables()
            .into_iter()
            .filter_map(|offset| {
                if offset < num_args {
                    None
                } else {
                    Some(offset - num_args)
                }
            })
            .collect()
    }
    fn dynamic_dependencies(&self) -> DependencyList {
        DependencyList::empty()
    }
    fn is_static(&self) -> bool {
        true
    }
    fn is_atomic(&self) -> bool {
        false
    }
}
impl<T: Expression + Rewritable<T>> Rewritable<T> for LambdaTerm<T> {
    fn subexpressions(&self) -> Vec<&T> {
        once(&self.body).chain(self.body.subexpressions()).collect()
    }
    fn substitute_static(
        &self,
        substitutions: &Substitutions<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.body
            .substitute_static(
                &substitutions.offset(self.num_args),
                factory,
                allocator,
                cache,
            )
            .map(|body| factory.create_lambda_term(self.num_args, body))
    }
    fn substitute_dynamic(
        &self,
        state: &impl DynamicState<T>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        self.body
            .substitute_dynamic(state, factory, allocator, cache)
            .map(|body| factory.create_lambda_term(self.num_args, body))
    }
    fn hoist_free_variables(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
    ) -> Option<T> {
        let free_variables = self.free_variables().into_iter().collect::<Vec<_>>();
        let hoisted_body = self.body.hoist_free_variables(factory, allocator);
        if free_variables.is_empty() {
            hoisted_body.map(|body| factory.create_lambda_term(self.num_args, body))
        } else {
            let mut stack_offsets = free_variables;
            stack_offsets.sort();
            let substitutions = stack_offsets
                .iter()
                .enumerate()
                .map(|(arg_offset, target_offset)| {
                    (
                        *target_offset,
                        factory.create_static_variable_term(arg_offset),
                    )
                })
                .collect::<Vec<_>>();
            let arg_values = stack_offsets
                .into_iter()
                .rev()
                .map(|offset| factory.create_static_variable_term(offset));
            let substitutions = Substitutions::named(&substitutions, None).offset(self.num_args);
            let body = hoisted_body.as_ref().unwrap_or(&self.body);
            match body.substitute_static(
                &substitutions,
                factory,
                allocator,
                &mut NoopCache::default(),
            ) {
                Some(body) => Some(factory.create_partial_application_term(
                    factory.create_lambda_term(arg_values.len() + self.num_args, body),
                    allocator.create_list(arg_values),
                )),
                None => hoisted_body.map(|body| factory.create_lambda_term(self.num_args, body)),
            }
        }
    }
    fn normalize(
        &self,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Option<T> {
        let normalized_body = self.body.normalize(factory, allocator, cache);
        let eta_reduced_body = apply_eta_reduction(
            normalized_body.as_ref().unwrap_or(&self.body),
            self.num_args,
            factory,
        );
        eta_reduced_body
            .and_then(|eta_reduced_body| {
                eta_reduced_body
                    .normalize(factory, allocator, cache)
                    .or_else(|| Some(eta_reduced_body.clone()))
            })
            .or(normalized_body.map(|body| factory.create_lambda_term(self.num_args, body)))
    }
}
impl<T: Expression + Rewritable<T>> Applicable<T> for LambdaTerm<T> {
    fn arity(&self) -> Option<Arity> {
        Some(Arity::from(0, self.num_args, None))
    }
    fn apply(
        &self,
        args: impl IntoIterator<Item = T, IntoIter = impl ExactSizeIterator<Item = T>>,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        cache: &mut impl EvaluationCache<T>,
    ) -> Result<T, String> {
        let num_args = self.num_args;
        if num_args == 0 || self.body.capture_depth() == 0 {
            Ok(self.body.clone())
        } else {
            let substitutions = args
                .into_iter()
                .take(num_args)
                .enumerate()
                .map(|(index, arg)| ((num_args - index - 1), arg))
                .collect::<Vec<_>>();
            let substitutions =
                Substitutions::named(&substitutions, Some(ScopeOffset::Unwrap(num_args)));
            Ok(self
                .body
                .substitute_static(&substitutions, factory, allocator, cache)
                .unwrap_or_else(|| self.body.clone()))
        }
    }
}
impl<T: Expression> std::fmt::Display for LambdaTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function:{}>", self.num_args)
    }
}
impl<T: Expression> serde::Serialize for LambdaTerm<T> {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        Err(serde::ser::Error::custom(format!(
            "Unable to serialize term: {}",
            self
        )))
    }
}

fn apply_eta_reduction<'a, T: Expression>(
    body: &'a T,
    num_args: StackOffset,
    factory: &'a impl ExpressionFactory<T>,
) -> Option<&'a T> {
    match factory.match_application_term(body) {
        Some(term)
            if term.target().capture_depth() == 0
                && term.args().len() <= num_args
                && term.args().iter().enumerate().all(|(index, arg)| {
                    match factory.match_static_variable_term(arg) {
                        Some(term) => term.offset() == num_args - index - 1,
                        _ => false,
                    }
                }) =>
        {
            Some(term.target())
        }
        _ => None,
    }
}
impl<T: Expression + Rewritable<T> + Reducible<T> + Compile<T>> Compile<T> for LambdaTerm<T> {
    fn compile(
        &self,
        _eager: VarArgs,
        _stack_offset: StackOffset,
        factory: &impl ExpressionFactory<T>,
        allocator: &impl HeapAllocator<T>,
        compiler: &mut Compiler,
    ) -> Result<(Program, NativeFunctionRegistry<T>), String> {
        let hash = hash_object(self);
        let num_args = self.num_args;
        let (target_address, native_functions) =
            match compiler.retrieve_compiled_chunk_address(hash) {
                Some(address) => (address, NativeFunctionRegistry::default()),
                None => {
                    let (compiled_body, native_functions) =
                        self.body
                            .compile(VarArgs::Eager, 0, factory, allocator, compiler)?;
                    let address = compiler.store_compiled_chunk(
                        hash,
                        Program::new(
                            once(Instruction::Function {
                                hash,
                                arity: num_args,
                                variadic: false,
                            })
                            .chain(compiled_body)
                            .chain(if num_args > 0 {
                                Some(Instruction::Squash { depth: num_args })
                            } else {
                                None
                            })
                            .chain(once(Instruction::Return)),
                        ),
                    );
                    (address, native_functions)
                }
            };
        Ok((
            Program::new(once(Instruction::PushFunction {
                target: target_address,
            })),
            native_functions,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        allocator::DefaultAllocator,
        cache::SubstitutionCache,
        core::{
            DependencyList, Evaluate, EvaluationResult, ExpressionFactory,
            HeapAllocator, Rewritable, StateCache,
        },
        lang::{BuiltinTerm, TermFactory, ValueTerm},
        parser::sexpr::parse,
    };

    #[test]
    fn hoist_lambda_variables() {
        let factory = TermFactory::default();
        let allocator = DefaultAllocator::default();

        let input = "
            (lambda (two one zero)
                (lambda ()
                    (+ (+ zero one) two)))";
        let expression = parse(input, &factory, &allocator).unwrap();
        let result = expression.hoist_free_variables(&factory, &allocator);
        assert_eq!(
            result,
            Some(factory.create_lambda_term(
                3,
                factory.create_partial_application_term(
                    factory.create_lambda_term(
                        3,
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Add),
                                    allocator.create_pair(
                                        factory.create_static_variable_term(0),
                                        factory.create_static_variable_term(1),
                                    ),
                                ),
                                factory.create_static_variable_term(2),
                            ),
                        ),
                    ),
                    allocator.create_list(vec![
                        factory.create_static_variable_term(2),
                        factory.create_static_variable_term(1),
                        factory.create_static_variable_term(0),
                    ]),
                )
            )),
        );
        let expression = factory.create_application_term(
            factory.create_application_term(
                result.unwrap(),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(1)),
                    factory.create_value_term(ValueTerm::Int(2)),
                    factory.create_value_term(ValueTerm::Int(3)),
                ]),
            ),
            allocator.create_empty_list(),
        );
        assert_eq!(
            expression.evaluate(
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new()
            ),
            Some(EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(1 + 2 + 3)),
                DependencyList::empty()
            ))
        );

        let input = "
            (lambda (three two one)
                (lambda (zero)
                    (+ (+ (+ zero one) two) three)))";
        let expression = parse(input, &factory, &allocator).unwrap();
        let result = expression.hoist_free_variables(&factory, &allocator);
        assert_eq!(
            result,
            Some(factory.create_lambda_term(
                3,
                factory.create_partial_application_term(
                    factory.create_lambda_term(
                        4,
                        factory.create_application_term(
                            factory.create_builtin_term(BuiltinTerm::Add),
                            allocator.create_pair(
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Add),
                                    allocator.create_pair(
                                        factory.create_application_term(
                                            factory.create_builtin_term(BuiltinTerm::Add),
                                            allocator.create_pair(
                                                factory.create_static_variable_term(0),
                                                factory.create_static_variable_term(1),
                                            ),
                                        ),
                                        factory.create_static_variable_term(2),
                                    ),
                                ),
                                factory.create_static_variable_term(3),
                            ),
                        ),
                    ),
                    allocator.create_list(vec![
                        factory.create_static_variable_term(2),
                        factory.create_static_variable_term(1),
                        factory.create_static_variable_term(0),
                    ]),
                ),
            )),
        );
        let expression = factory.create_application_term(
            factory.create_application_term(
                result.unwrap(),
                allocator.create_list(vec![
                    factory.create_value_term(ValueTerm::Int(1)),
                    factory.create_value_term(ValueTerm::Int(2)),
                    factory.create_value_term(ValueTerm::Int(3)),
                ]),
            ),
            allocator.create_list(vec![factory.create_value_term(ValueTerm::Int(4))]),
        );
        assert_eq!(
            expression.evaluate(
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new()
            ),
            Some(EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(1 + 2 + 3 + 4)),
                DependencyList::empty()
            ))
        );

        let input = "
        (lambda (five four three)
            (lambda (two)
                (lambda (one zero)
                    (+ (+ (+ (+ (+ zero one) two) three) four) five))))";
        let expression = parse(input, &factory, &allocator).unwrap();
        let result = expression.hoist_free_variables(&factory, &allocator);
        assert_eq!(
            result,
            Some(factory.create_lambda_term(
                3,
                factory.create_partial_application_term(
                    factory.create_lambda_term(
                        4,
                        factory.create_partial_application_term(
                            factory.create_lambda_term(
                                6,
                                factory.create_application_term(
                                    factory.create_builtin_term(BuiltinTerm::Add),
                                    allocator.create_pair(
                                        factory.create_application_term(
                                            factory.create_builtin_term(BuiltinTerm::Add),
                                            allocator.create_pair(
                                                factory.create_application_term(
                                                    factory.create_builtin_term(BuiltinTerm::Add),
                                                    allocator.create_pair(
                                                        factory.create_application_term(
                                                            factory.create_builtin_term(BuiltinTerm::Add),
                                                            allocator.create_pair(
                                                                factory.create_application_term(
                                                                    factory.create_builtin_term(BuiltinTerm::Add),
                                                                    allocator.create_pair(
                                                                        factory.create_static_variable_term(0),
                                                                        factory.create_static_variable_term(1),
                                                                    ),
                                                                ),
                                                                factory.create_static_variable_term(2),
                                                            ),
                                                        ),
                                                        factory.create_static_variable_term(3),
                                                    ),
                                                ),
                                                factory.create_static_variable_term(4),
                                            ),
                                        ),
                                        factory.create_static_variable_term(5),
                                    ),
                                ),
                            ),
                            allocator.create_list(vec![
                                factory.create_static_variable_term(3),
                                factory.create_static_variable_term(2),
                                factory.create_static_variable_term(1),
                                factory.create_static_variable_term(0),
                            ]),
                        ),
                    ),
                    allocator.create_list(vec![
                        factory.create_static_variable_term(2),
                        factory.create_static_variable_term(1),
                        factory.create_static_variable_term(0),
                    ])
                )
            )),
        );
        let expression = factory.create_application_term(
            factory.create_application_term(
                factory.create_application_term(
                    result.unwrap(),
                    allocator.create_list(vec![
                        factory.create_value_term(ValueTerm::Int(1)),
                        factory.create_value_term(ValueTerm::Int(2)),
                        factory.create_value_term(ValueTerm::Int(3)),
                    ]),
                ),
                allocator.create_list(vec![factory.create_value_term(ValueTerm::Int(4))]),
            ),
            allocator.create_list(vec![
                factory.create_value_term(ValueTerm::Int(5)),
                factory.create_value_term(ValueTerm::Int(6)),
            ]),
        );
        assert_eq!(
            expression.evaluate(
                &StateCache::default(),
                &factory,
                &allocator,
                &mut SubstitutionCache::new()
            ),
            Some(EvaluationResult::new(
                factory.create_value_term(ValueTerm::Int(1 + 2 + 3 + 4 + 5 + 6)),
                DependencyList::empty()
            ))
        );
    }
}
