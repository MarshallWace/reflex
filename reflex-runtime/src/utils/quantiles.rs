// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Chris Campbell <c.campbell@mwam.com> https://github.com/c-campbell-mwam
use std::iter::once;

use metrics::{gauge, SharedString};

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct QuantileBucket(pub f64);
impl From<f64> for QuantileBucket {
    fn from(value: f64) -> Self {
        Self(value)
    }
}
impl From<QuantileBucket> for f64 {
    fn from(value: QuantileBucket) -> Self {
        let QuantileBucket(value) = value;
        value
    }
}
impl std::fmt::Display for QuantileBucket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(value) = self;
        write!(f, "{}", value)
    }
}

pub(crate) fn generate_quantile_metric_labels<const NUM_BUCKETS: usize>(
    buckets: &[QuantileBucket; NUM_BUCKETS],
    metric_labels: &[(SharedString, SharedString)],
) -> [Vec<(SharedString, SharedString)>; NUM_BUCKETS] {
    (*buckets).map(|quantile| {
        metric_labels
            .iter()
            .map(|(key, value)| (key.clone(), value.clone()))
            .chain(once(("quantile".into(), format!("{}", quantile).into())))
            .collect()
    })
}

pub(crate) fn compute_bucketed_values<const NUM_BUCKETS: usize>(
    values: impl IntoIterator<Item = f64>,
    buckets: &[QuantileBucket; NUM_BUCKETS],
    error: f64,
) -> Option<[f64; NUM_BUCKETS]> {
    let mut ckms = quantiles::ckms::CKMS::<f64>::new(error);
    for i in values {
        ckms.insert(i as f64);
    }
    let mut values = [Default::default(); NUM_BUCKETS];
    for (index, quantile) in buckets.iter().copied().enumerate() {
        let (_, value) = ckms.query(quantile.into())?;
        values[index] = value;
    }
    Some(values)
}

pub(crate) fn publish_quantile_bucketed_metric<const NUM_BUCKETS: usize>(
    values: impl IntoIterator<Item = f64>,
    metric_name: &'static str,
    buckets: &[QuantileBucket; NUM_BUCKETS],
    metric_labels: &[Vec<(SharedString, SharedString)>; NUM_BUCKETS],
) -> Option<()> {
    let values = compute_bucketed_values(values, buckets, 0.01)?;
    for (value, metric_labels) in values.iter().zip(metric_labels.iter()) {
        gauge!(metric_name, *value, metric_labels);
    }
    Some(())
}
