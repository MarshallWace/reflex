// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::time::Duration;

pub trait ReconnectTimeout {
    fn duration(&self, attempt_index: usize) -> Option<Duration>;
}

impl<_Self> ReconnectTimeout for _Self
where
    Self: Fn(usize) -> Option<Duration>,
{
    fn duration(&self, attempt_index: usize) -> Option<Duration> {
        self(attempt_index)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NoopReconnectTimeout;
impl ReconnectTimeout for NoopReconnectTimeout {
    fn duration(&self, _attempt_index: usize) -> Option<Duration> {
        None
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FibonacciReconnectTimeout {
    pub units: Duration,
    pub max_timeout: Duration,
}
impl ReconnectTimeout for FibonacciReconnectTimeout {
    fn duration(&self, attempt_index: usize) -> Option<Duration> {
        let max_timeout = self.max_timeout;
        let scale = self.units;
        let max_value = div_ceil(max_timeout.as_nanos(), scale.as_nanos()) as u32;
        let value = fib(attempt_index as u32, max_value);
        Some(match value {
            Some(value) => value * scale,
            None => max_timeout,
        })
    }
}
fn div_ceil(numerator: u128, denominator: u128) -> u128 {
    if numerator == 0 || denominator == 0 {
        0
    } else {
        ((numerator - 1) / denominator) + 1
    }
}
fn fib(n: u32, max: u32) -> Option<u32> {
    if n < 2 {
        if n < max {
            Some(n)
        } else {
            None
        }
    } else {
        let mut n = n - 2;
        let mut n1 = 1;
        let mut n2 = 0;
        while n > 0 {
            let value = n1 + n2;
            if value >= max {
                break;
            }
            n2 = n1;
            n1 = value;
            n -= 1;
        }
        let value = n1 + n2;
        if value >= max {
            None
        } else {
            Some(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    #[test]
    fn fibonacci_reconnect_timeout() {
        let timeout = FibonacciReconnectTimeout {
            units: Duration::from_secs(1),
            max_timeout: Duration::from_secs(7),
        };
        assert_eq!(timeout.duration(0), Some(Duration::from_secs(0)));
        assert_eq!(timeout.duration(1), Some(Duration::from_secs(1)));
        assert_eq!(timeout.duration(2), Some(Duration::from_secs(1)));
        assert_eq!(timeout.duration(3), Some(Duration::from_secs(2)));
        assert_eq!(timeout.duration(4), Some(Duration::from_secs(3)));
        assert_eq!(timeout.duration(5), Some(Duration::from_secs(5)));
        assert_eq!(timeout.duration(6), Some(Duration::from_secs(7)));
        assert_eq!(timeout.duration(7), Some(Duration::from_secs(7)));

        let timeout = FibonacciReconnectTimeout {
            units: Duration::from_secs(2),
            max_timeout: Duration::from_secs(15),
        };
        assert_eq!(timeout.duration(0), Some(Duration::from_secs(0)));
        assert_eq!(timeout.duration(1), Some(Duration::from_secs(2)));
        assert_eq!(timeout.duration(2), Some(Duration::from_secs(2)));
        assert_eq!(timeout.duration(3), Some(Duration::from_secs(4)));
        assert_eq!(timeout.duration(4), Some(Duration::from_secs(6)));
        assert_eq!(timeout.duration(5), Some(Duration::from_secs(10)));
        assert_eq!(timeout.duration(6), Some(Duration::from_secs(15)));
        assert_eq!(timeout.duration(7), Some(Duration::from_secs(15)));

        let timeout = FibonacciReconnectTimeout {
            units: Duration::from_millis(500),
            max_timeout: Duration::from_millis(1750),
        };
        assert_eq!(timeout.duration(0), Some(Duration::from_secs(0)));
        assert_eq!(timeout.duration(1), Some(Duration::from_millis(500)));
        assert_eq!(timeout.duration(2), Some(Duration::from_millis(500)));
        assert_eq!(timeout.duration(3), Some(Duration::from_millis(1000)));
        assert_eq!(timeout.duration(4), Some(Duration::from_millis(1500)));
        assert_eq!(timeout.duration(5), Some(Duration::from_millis(1750)));
        assert_eq!(timeout.duration(6), Some(Duration::from_millis(1750)));
    }
}
