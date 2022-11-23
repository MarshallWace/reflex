// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::{marker::PhantomData, ops::Deref};

use reflex_dispatcher::{Action, TaskFactory};
use reflex_scheduler::tokio::{TokioCommand, TokioSchedulerLogger};

use crate::logger::{
    formatter::{LogFormatter, LogWriter},
    ActionLogger,
};

#[derive(Debug)]
pub struct PrefixedLogFormatter<T: LogFormatter> {
    prefix: &'static str,
    formatter: T,
}
impl<T: LogFormatter> PrefixedLogFormatter<T> {
    pub fn new(prefix: &'static str, formatter: T) -> Self {
        Self { prefix, formatter }
    }
}
impl<T: LogFormatter> Clone for PrefixedLogFormatter<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix,
            formatter: self.formatter.clone(),
        }
    }
}
impl<T: LogFormatter> Copy for PrefixedLogFormatter<T> where T: Copy {}
impl<T: LogFormatter> LogFormatter for PrefixedLogFormatter<T> {
    type Message = T::Message;
    type Writer<'a> = PrefixedLogWriter<T::Writer<'a>>
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, message: &'a Self::Message) -> Option<Self::Writer<'a>> {
        self.formatter
            .format(message)
            .map(|inner| PrefixedLogWriter {
                prefix: self.prefix,
                inner,
            })
    }
}
pub struct PrefixedLogWriter<T: LogWriter> {
    prefix: &'static str,
    inner: T,
}
impl<T: LogWriter> LogWriter for PrefixedLogWriter<T> {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "[{}] ", self.prefix)?;
        self.inner.write(f)
    }
}

pub struct FormattedActionLogger<TOut, TFormatter, TAction, TTask>
where
    TFormatter: LogFormatter,
    TOut: std::io::Write,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    formatter: TFormatter,
    output: TOut,
    _action: PhantomData<TAction>,
    _task: PhantomData<TTask>,
}
impl<TOut, TFormatter, TAction, TTask> FormattedActionLogger<TOut, TFormatter, TAction, TTask>
where
    TOut: std::io::Write,
    TFormatter: LogFormatter,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn new(output: TOut, formatter: TFormatter) -> Self {
        Self {
            formatter,
            output,
            _action: PhantomData,
            _task: PhantomData,
        }
    }
    fn write<'a>(
        &'a mut self,
        writer: <TFormatter as LogFormatter>::Writer<'a>,
    ) -> std::io::Result<()> {
        writer.write(&mut self.output)?;
        self.flush()
    }
    fn flush(&mut self) -> std::io::Result<()> {
        write!(&mut self.output, "\n")
    }
}
impl<TFormatter, TAction, TTask> FormattedActionLogger<std::io::Stdout, TFormatter, TAction, TTask>
where
    TFormatter: LogFormatter,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stdout(formatter: TFormatter) -> Self {
        Self::new(std::io::stdout(), formatter)
    }
}
impl<TFormatter, TAction, TTask> Clone
    for FormattedActionLogger<std::io::Stdout, TFormatter, TAction, TTask>
where
    TFormatter: LogFormatter + Clone,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self::stdout(self.formatter.clone())
    }
}
impl<TFormatter, TAction, TTask> FormattedActionLogger<std::io::Stderr, TFormatter, TAction, TTask>
where
    TFormatter: LogFormatter,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    pub fn stderr(formatter: TFormatter) -> Self {
        Self::new(std::io::stderr(), formatter)
    }
}

impl<TFormatter, TAction, TTask> Clone
    for FormattedActionLogger<std::io::Stderr, TFormatter, TAction, TTask>
where
    TFormatter: LogFormatter + Clone,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    fn clone(&self) -> Self {
        Self::stderr(self.formatter.clone())
    }
}

impl<TOut, TFormatter, TAction, TTask> ActionLogger
    for FormattedActionLogger<TOut, TFormatter, TAction, TTask>
where
    TOut: std::io::Write,
    TFormatter: LogFormatter<Message = TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    fn log(&mut self, action: &Self::Action) {
        if let Some(writer) = self.formatter.format(action) {
            let _ = self.write(writer);
        }
    }
}

impl<TOut, TFormatter, TAction, TTask> TokioSchedulerLogger
    for FormattedActionLogger<TOut, TFormatter, TAction, TTask>
where
    TOut: std::io::Write,
    TFormatter: LogFormatter<Message = TAction>,
    TAction: Action,
    TTask: TaskFactory<TAction, TTask>,
{
    type Action = TAction;
    type Task = TTask;

    fn log(&mut self, command: &TokioCommand<Self::Action, Self::Task>) {
        match command {
            TokioCommand::Send { pid: _, message } => {
                let action = message.deref();
                if let Some(writer) = self.formatter.format(action) {
                    let _ = self.write(writer);
                }
            }
            _ => {}
        }
    }
}
