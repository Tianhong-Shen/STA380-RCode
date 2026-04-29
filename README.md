# STA380-RCode

R practice repository for **STA380H5 (Computational Statistics)**, covering simulation methods, Monte Carlo inference, resampling, optimization, and final-exam review scripts.

## Repository Structure

- `Term Test 1/` — random variate generation and foundational Monte Carlo.
- `Term Test 2/` — variance reduction, estimation diagnostics, and hypothesis testing simulation.
- `Final Exam/` — resampling, optimization, EM/Newton-Raphson, and formula-sheet reference.
- `STA380-RCode.Rproj` — RStudio project configuration.
- `LICENSE` — MIT license.

## Detailed File Guide

## `Term Test 1/`

### `Unit1_Inverse_Transform.R`
Inverse-CDF simulation practice:
- Continuous examples (e.g., densities proportional to `x^2`, exponential, Weibull).
- Discrete inversion (custom support sets, geometric).
- Recursive CDF construction for binomial and custom PMFs.
- Quiz/test practice blocks and histogram checks.

### `Unit1_Acceptance_Rejection.R`
Acceptance-rejection sampling workflows:
- Baseline algorithm implementation with acceptance counters.
- Empirical acceptance-rate checks against theoretical `1/c`.
- Multiple target/proposal combinations (uniform and exponential proposals).
- Visualization of accepted points and practice problems.

### `Unit1_Transformation_and_Convolution.R`
Distribution construction by transformation/sums:
- Beta from gamma/exponential sums.
- Gamma from exponential sums.
- Practice for t- and F-related constructions.

### `Unit1_Mixture_Method.R`
Mixture-distribution simulation:
- Two-component and three-component normal mixtures.
- Categorical component selection via specified probabilities.
- Side-by-side comparisons between sampled mixtures and weighted combinations.

### `Unit2_simple_Monte_Carlo.R`
Simple Monte Carlo integration/expectation:
- Numerical estimation of integrals on bounded intervals.
- Comparisons with analytical values or `integrate()`.
- Includes `testthat` assertions in several examples.
- Additional practice on transformed-density probabilities.

## `Term Test 2/`

### `Matrix_Example.R`
Quick matrix refresher:
- Matrix creation (column-major vs row-wise filling).
- `rowSums`, `colSums`, `rowMeans`, `colMeans`.

### `Unit2_Hit_or_Miss.R`
Indicator-based probability estimation:
- Tail/interval probability estimation under normal, Weibull, gamma, and exponential settings.
- Standard error and confidence interval construction.
- Validation versus built-in CDF functions.

### `Unit2_Antithetic_Variables.R`
Variance reduction via antithetic variates:
- Integral estimation with paired uniforms `u` and `1-u`.
- Normal CDF approximation for positive and negative cutoffs.
- Practice problems comparing Monte Carlo estimates to known values.

### `Unit2_Importance_Sampling.R`
Importance sampling:
- Rare-event normal-tail estimation with exponential proposal.
- Integral estimation with normal/log-normal proposals.
- Comparison with simple Monte Carlo baselines.

### `Unit3_Monte_Carlo_Estimation.R`
Estimator behavior and inferential properties:
- Estimation of expectations and Monte Carlo standard error.
- Biased vs unbiased variance/SE formulas.
- Estimator comparison using MSE and Pitman closeness.
- Empirical confidence interval coverage experiments.

### `Unit3_Monte_Carlo_Hypothesis_Testing.R`
Simulation-based hypothesis-testing diagnostics:
- Type I error under null settings.
- Type II error and power under alternatives.
- Sensitivity to sample size, variance, and effect size.

## `Final Exam/`

### `Unit4_Bootstrap.R`
Bootstrap estimation with `sleepstudy` data:
- Bootstrap mean estimate.
- Bootstrap standard error and bias.
- Quantile and empirical bootstrap confidence intervals.

### `Unit4_Jackknife.R`
Jackknife resampling examples:
- Jackknife estimates for mean and variance.
- Jackknife bias estimation.
- Jackknife standard error for median.

### `Unit4_Permutation_Test.R`
Permutation test workflow:
- Two-sample distribution comparison via KS-style statistic.
- Monte Carlo permutation null distribution and p-value.
- Comparison with `ks.test()` output.

### `Unit6_Optimize.R`
Optimization and MLE computation:
- Univariate optimization with `optimize()`.
- Likelihood-based estimation using `optimize()` and `optim()`.
- MLE examples for Pareto-, gamma-, and normal-related parameters.

### `Unit6_Newton_Raphson.R`
Newton-Raphson numerical MLE:
- Iterative parameter updates via gradient/Hessian (using `numDeriv`).
- Gamma and normal likelihood examples.
- Convergence checks via Euclidean-step tolerance.

### `Unit6_EM_Algorithm.R`
EM algorithm demonstrations with missing values:
- Poisson mean estimation with one missing observation.
- Normal mean estimation (known sigma) with one missing observation.
- Iterative fixed-point update until tolerance is met.

### `STA380 Final Exam Formula Sheet.pdf`
Reference PDF for final exam formulas and summary notes.

## Requirements

- **R** (recommended `>= 4.0`).
- Optional packages used by scripts:
  - `testthat`
  - `numDeriv`
  - `extraDistr`
  - `lme4`

Install packages if needed:

```r
install.packages(c("testthat", "numDeriv", "extraDistr", "lme4"))
```

## How to Run

Because scripts are educational and section-based, the typical workflow is:

1. Open `STA380-RCode.Rproj` in RStudio.
2. Open a script for the relevant unit.
3. Run blocks top-to-bottom (or section-by-section).
4. Optionally set a seed for reproducibility, e.g. `set.seed(123)`.

## Notes

- Many scripts are self-contained demonstrations rather than packaged functions.
- Several scripts include plots (`hist`, `curve`, `plot`, `points`) for visual checks.
- Randomized outputs vary unless seeds are fixed.
- Some scripts reuse variable names across sections; re-run clean sessions when needed.

## License

This repository is licensed under the MIT License. See `LICENSE` for details.
