use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct TimingStats {
    /// In the order they were taken.
    pub samples_ms: Vec<f64>,
    pub mean_ms: f64,
    pub median_ms: f64,
    /// The median absolute deviation from `median_ms`.
    #[serde(default)]
    pub mad_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub margin_of_error_ms: Option<f64>,
}

impl TimingStats {
    pub fn new(samples_ms: Vec<f64>) -> Self {
        let mut sorted = samples_ms.clone();
        sorted.sort_by(f64::total_cmp);
        let mean_ms = sorted.iter().sum::<f64>() / sorted.len() as f64;
        let median_ms = median(&sorted);
        let mut deviations = sorted
            .iter()
            .map(|sample| (sample - median_ms).abs())
            .collect::<Vec<_>>();
        deviations.sort_by(f64::total_cmp);

        Self {
            mean_ms,
            median_ms,
            mad_ms: median(&deviations),
            min_ms: sorted[0],
            max_ms: sorted[sorted.len() - 1],
            margin_of_error_ms: margin_of_error_ms(&sorted, mean_ms),
            samples_ms,
        }
    }
}

fn median(sorted: &[f64]) -> f64 {
    let middle = sorted.len() / 2;
    match sorted.len() % 2 {
        0 => (sorted[middle - 1] + sorted[middle]) / 2.0,
        _ => sorted[middle],
    }
}

fn margin_of_error_ms(samples_ms: &[f64], mean_ms: f64) -> Option<f64> {
    let sample_count = samples_ms.len();
    if sample_count < 2 {
        return None;
    }

    let variance = samples_ms
        .iter()
        .map(|sample| (sample - mean_ms).powi(2))
        .sum::<f64>()
        / (sample_count - 1) as f64;
    let standard_error = (variance / sample_count as f64).sqrt();

    Some(t_critical_95(sample_count - 1) * standard_error)
}

fn t_critical_95(degrees_of_freedom: usize) -> f64 {
    const VALUES: [f64; 30] = [
        12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160,
        2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 2.060, 2.056,
        2.052, 2.048, 2.045, 2.042,
    ];

    VALUES
        .get(degrees_of_freedom.saturating_sub(1))
        .copied()
        .unwrap_or(1.96)
}
