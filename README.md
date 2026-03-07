# STA380-RCode

This is a Repo for my STA380H5 S (Winter) Computational Statistics R code practice, including lecture examples, quiz practice, and term test Practice problems.

## Repository Overview

This repository is organized into two folders that roughly follow course progression:

- **Term Test 1/**: Random variate generation and foundational Monte Carlo methods.
- **Term Test 2/**: Variance reduction, estimation accuracy, confidence intervals, and hypothesis testing.

## Folder and File Guide

### Term Test 1

#### `Unit1_Inverse_Transform.R`
Inverse transform sampling examples and practice problems, including:

- Continuous distributions (e.g., densities like \(3x^2\), exponential, Weibull).
- Discrete simulation using CDF inversion (e.g., geometric, custom discrete RVs).
- Recursive CDF construction for binomial and a custom PMF.
- Quiz and term-test practice variants.

#### `Unit1_Acceptance_Rejection.R`
Acceptance-rejection sampling examples and exercises:

- Core acceptance-rejection workflow with tracking of accepted samples.
- Empirical acceptance-rate checks against theoretical \(1/c\).
- Multiple target/proposal settings (uniform and exponential proposals).
- Quiz and term-test practice applications.

#### `Unit1_Transformation_and_Convolution.R`
Transformation/convolution-based simulation constructions:

- Beta via ratio of gamma/exponential sums.
- Gamma via sums of exponentials.
- Practice examples involving \(t\)- and \(F\)-type constructions.

#### `Unit1_Mixture_Method.R`
Mixture method examples:

- Two-component and three-component normal mixtures.
- Categorical component selection with specified probabilities.
- Practice problems comparing sampled mixtures vs weighted combinations.

#### `Unit2_simple_Monte_Carlo.R`
Simple Monte Carlo integration/expectation estimation:

- Estimation of definite integrals over bounded intervals.
- Validation against closed-form or `integrate()` values.
- Use of `testthat::expect_equal()` in several examples.
- Additional practice problems including transformed-density probability estimation.

### Term Test 2

#### `Matrix_Example.R`
Quick matrix operations refresher:

- Matrix creation (default column-major and row-wise filling).
- Row/column sums and means.

#### `Unit2_Hit_or_Miss.R`
Hit-or-miss Monte Carlo probability estimation:

- Estimating tail and interval probabilities for normal and Weibull/gamma contexts.
- Confidence interval construction for Bernoulli-indicator estimates.
- Comparisons against analytical CDF-based values.

#### `Unit2_Antithetic_Variables.R`
Antithetic variates for variance reduction:

- Integral estimation with paired uniforms \(u\) and \(1-u\).
- Approximation of normal CDF values for positive and negative cutoffs.
- Practice problem using antithetic construction with exponential-type terms.

#### `Unit2_Importance_Sampling.R`
Importance sampling examples:

- Rare-event normal tail probability estimation using exponential proposal.
- Integral estimation using normal and log-normal importance functions.
- Comparisons of simple MC vs importance sampling estimates.

#### `Unit3_Monte_Carlo_Estimation.R`
Monte Carlo estimator behavior and inference:

- Estimation of expectations and standard errors.
- Bias/unbiased variance/SE calculations.
- Estimator comparison via MSE and Pitman closeness.
- Empirical confidence interval coverage studies.
- Additional practice on variance expressions and Monte Carlo variance checks.

#### `Unit3_Monte_Carlo_Hypothesis_Testing.R`
Monte Carlo hypothesis testing diagnostics:

- Type I error simulation under null.
- Type II error and power simulation under alternative.
- Sensitivity notes on effect size, variance, and sample size.

## Requirements

- **R** (recommended >= 4.0)
- Optional package used in multiple scripts:
  - `testthat`

Install package if needed:

```r
install.packages("testthat")
```

## Notes

- Scripts are primarily educational and demonstration-oriented, with many independent code blocks intended to run section-by-section.
- Some scripts generate plots (`hist`, `curve`, `plot`, `points`) for visual validation.
- Random outputs vary unless you set a seed (e.g., `set.seed(123)`).
