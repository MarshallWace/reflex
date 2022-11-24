// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::{marker::PhantomData, time::SystemTime};

use crate::utils::datetime::format_datetime;

pub trait LogFormatter {
    type Message;
    type Writer<'a>: LogWriter
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, message: &'a Self::Message) -> Option<Self::Writer<'a>>;
}

pub trait LogWriter {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()>;
}

impl<T: LogFormatter> LogFormatter for Option<T> {
    type Message = T::Message;
    type Writer<'a> = T::Writer<'a>
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, message: &'a Self::Message) -> Option<Self::Writer<'a>> {
        match self {
            Some(inner) => inner.format(message),
            None => None,
        }
    }
}

#[derive(Copy, Debug)]
struct NoopLogFormatter<T> {
    _message: PhantomData<T>,
}
impl<T> Default for NoopLogFormatter<T> {
    fn default() -> Self {
        Self {
            _message: PhantomData,
        }
    }
}
impl<T> Clone for NoopLogFormatter<T> {
    fn clone(&self) -> Self {
        Self {
            _message: PhantomData,
        }
    }
}
impl<T> LogFormatter for NoopLogFormatter<T> {
    type Message = T;
    type Writer<'a> = NoopLogWriter
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, _message: &'a Self::Message) -> Option<Self::Writer<'a>> {
        None
    }
}

#[derive(Clone, Copy, Debug)]
struct NoopLogWriter;
impl LogWriter for NoopLogWriter {
    fn write(&self, _f: &mut impl std::io::Write) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstantLogFormatter<T> {
    message: &'static str,
    _message: PhantomData<T>,
}
impl<T> ConstantLogFormatter<T> {
    pub fn new(message: &'static str) -> Self {
        Self {
            message,
            _message: PhantomData,
        }
    }
}
impl<T> LogFormatter for ConstantLogFormatter<T> {
    type Message = T;
    type Writer<'a> = ConstantLogWriter
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, _message: &'a Self::Message) -> Option<Self::Writer<'a>> {
        Some(ConstantLogWriter {
            message: self.message,
        })
    }
}
#[derive(Clone, Copy, Debug)]
pub struct ConstantLogWriter {
    message: &'static str,
}
impl LogWriter for ConstantLogWriter {
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        write!(f, "{}", &self.message)
    }
}

#[derive(Debug)]
pub struct ChainedLogFormatter<T1, T2> {
    left: T1,
    right: T2,
}
impl<T1, T2> ChainedLogFormatter<T1, T2> {
    pub fn new(left: T1, right: T2) -> Self {
        Self { left, right }
    }
}
impl<T1, T2> Clone for ChainedLogFormatter<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2> Copy for ChainedLogFormatter<T1, T2>
where
    T1: Copy,
    T2: Copy,
{
}
impl<T1, T2, T> LogFormatter for ChainedLogFormatter<T1, T2>
where
    T1: LogFormatter<Message = T>,
    T2: LogFormatter<Message = T>,
{
    type Message = T;
    type Writer<'a> = ChainedLogWriter<T1::Writer<'a>, T2::Writer<'a>>
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, message: &'a T) -> Option<Self::Writer<'a>> {
        match (self.left.format(message), self.right.format(message)) {
            (Some(left), Some(right)) => Some(ChainedLogWriter { left, right }),
            _ => None,
        }
    }
}
#[derive(Debug)]
pub struct ChainedLogWriter<T1, T2> {
    left: T1,
    right: T2,
}
impl<T1, T2> Clone for ChainedLogWriter<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.clone(),
        }
    }
}
impl<T1, T2> Copy for ChainedLogWriter<T1, T2>
where
    T1: Copy,
    T2: Copy,
{
}
impl<T1, T2> LogWriter for ChainedLogWriter<T1, T2>
where
    T1: LogWriter,
    T2: LogWriter,
{
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        self.left.write(f)?;
        self.right.write(f)?;
        Ok(())
    }
}

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

#[derive(Debug)]
pub struct TimestampedLogFormatter<T> {
    inner: T,
    format: &'static str,
}
impl<T> TimestampedLogFormatter<T> {
    pub fn new(format: &'static str, formatter: T) -> Self {
        Self {
            inner: formatter,
            format,
        }
    }
}
impl<T> Clone for TimestampedLogFormatter<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            format: self.format,
        }
    }
}
impl<T> Copy for TimestampedLogFormatter<T> where T: Copy {}
impl<T, V> LogFormatter for TimestampedLogFormatter<T>
where
    T: LogFormatter<Message = V>,
{
    type Message = V;
    type Writer<'a> = TimestampedLogWriter<T::Writer<'a>>
    where
        Self: 'a,
        Self::Message: 'a;
    fn format<'a>(&self, message: &'a V) -> Option<Self::Writer<'a>> {
        self.inner
            .format(message)
            .map(|inner| TimestampedLogWriter {
                inner,
                format: self.format,
            })
    }
}
#[derive(Debug)]
pub struct TimestampedLogWriter<T> {
    inner: T,
    format: &'static str,
}
impl<T> Clone for TimestampedLogWriter<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            format: self.format,
        }
    }
}
impl<T> Copy for TimestampedLogWriter<T> where T: Copy {}
impl<T> LogWriter for TimestampedLogWriter<T>
where
    T: LogWriter,
{
    fn write(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        let label = format_datetime(SystemTime::now(), self.format);
        write!(f, "{}", label)?;
        self.inner.write(f)?;
        Ok(())
    }
}
