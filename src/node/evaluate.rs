// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use crate::{
    env::Env,
    expression::{EvaluationResult, EvaluationResultType, Expression, RuntimeState},
    node::Node,
    signal::{Signal, SignalResult},
};

pub type EvaluateError = String;
pub type EvaluateResult = Result<Expression<Node>, EvaluateError>;

pub trait Evaluate0 {
    fn run(&self) -> EvaluateResult;
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        Some(match self.run() {
            Err(message) => EvaluationResult::signal(Signal::error(message)),
            Ok(result) => result.evaluate(env, state),
        })
    }
}

pub trait Evaluate1 {
    fn dependencies(&self) -> &Expression<Node>;
    fn run(&self, dep1: &Expression<Node>) -> EvaluateResult;
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        let dep1 = self.dependencies();
        let (result, dependencies) = dep1.evaluate(env, state).unwrap();
        Some(match result {
            EvaluationResultType::Signal(signal) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            EvaluationResultType::Expression(expression) => match self.run(&expression) {
                Err(message) => EvaluationResult::signal_with_dependencies(
                    SignalResult::single(Signal::error(message)),
                    dependencies,
                ),
                Ok(result) => result.evaluate(env, state).add_dependencies(dependencies),
            },
        })
    }
}

pub trait Evaluate2 {
    fn dependencies(&self) -> (&Expression<Node>, &Expression<Node>);
    fn run(&self, dep1: &Expression<Node>, dep2: &Expression<Node>) -> EvaluateResult;
    fn evaluate(
        &self,
        env: &Env<Node>,
        state: &RuntimeState<Node>,
    ) -> Option<EvaluationResult<Node>> {
        let (dep1, dep2) = self.dependencies();
        let (result1, dependencies1) = dep1.evaluate(env, state).unwrap();
        let (result2, dependencies2) = dep2.evaluate(env, state).unwrap();
        let dependencies = dependencies1.extend(dependencies2);
        Some(match (result1, result2) {
            (EvaluationResultType::Signal(signal1), EvaluationResultType::Signal(signal2)) => {
                EvaluationResult::signal_with_dependencies(signal1.combine(signal2), dependencies)
            }
            (EvaluationResultType::Signal(signal), _) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            (_, EvaluationResultType::Signal(signal)) => {
                EvaluationResult::signal_with_dependencies(signal, dependencies)
            }
            (
                EvaluationResultType::Expression(expression1),
                EvaluationResultType::Expression(expression2),
            ) => match self.run(&expression1, &expression2) {
                Err(message) => EvaluationResult::signal_with_dependencies(
                    SignalResult::single(Signal::error(message)),
                    dependencies,
                ),
                Ok(result) => result.evaluate(env, state).add_dependencies(dependencies),
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expression::{DependencyList, EvaluationResult, RuntimeState},
        node::parser,
        signal::{CombinedSignal, Signal, SignalResult},
    };

    #[test]
    fn combined_signals() {
        let env = Env::new();
        let state = RuntimeState::new();
        let expression = parser::parse("(+ (pending) (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(result, EvaluationResult::signal(Signal::pending()));
        let expression = parser::parse("(+ (error \"foo\") (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::single(Signal::error(String::from("foo"))),
                DependencyList::new(),
            ),
        );
        let expression = parser::parse("(+ (error \"foo\") (error \"bar\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(CombinedSignal::join(
                    Signal::error(String::from("foo")),
                    Signal::error(String::from("bar")),
                )),
                DependencyList::new(),
            ),
        );
        let expression = parser::parse("(+ (pending) (error \"foo\"))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(CombinedSignal::join(
                    Signal::pending(),
                    Signal::error(String::from("foo")),
                )),
                DependencyList::new(),
            ),
        );
        let expression = parser::parse("(+ (error \"foo\") (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(CombinedSignal::join(
                    Signal::error(String::from("foo")),
                    Signal::pending(),
                )),
                DependencyList::new(),
            ),
        );
        let expression = parser::parse("(+ (pending) (+ (pending) (error \"foo\")))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(CombinedSignal::join(
                    Signal::pending(),
                    Signal::error(String::from("foo"))
                )),
                DependencyList::new(),
            ),
        );
        let expression = parser::parse("(+ (+ (error \"foo\") (pending)) (pending))").unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(CombinedSignal::join(
                    Signal::error(String::from("foo")),
                    Signal::pending()
                )),
                DependencyList::new(),
            ),
        );
        let expression =
            parser::parse("(+ (+ (error \"foo\") (pending)) (+ (error \"foo\") (error \"bar\")))")
                .unwrap();
        let result = expression.evaluate(&env, &state);
        assert_eq!(
            result,
            EvaluationResult::signal_with_dependencies(
                SignalResult::multiple(
                    CombinedSignal::join(Signal::error(String::from("foo")), Signal::pending())
                        .append(Signal::error(String::from("bar")))
                ),
                DependencyList::new(),
            ),
        );
    }
}
