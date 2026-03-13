# Leakage-Safe Covariate Handling in CPM

## Why This Matters

When covariates are regressed out on the full dataset before
cross-validation, held-out test information leaks into the training
pipeline. This can inflate or distort out-of-sample performance.

In `cpmr`, leakage-safe behavior means:

- fit covariate regression on each fold’s training split only;
- apply the learned regression parameters to both train and test data
  inside the same fold;
- keep other parameter-learning steps (for example scaling or model
  selection) inside CV as well.

## Minimal Example with Synthetic Data

``` r
library(cpmr)

set.seed(123)
n <- 80
p <- 120

# Synthetic connectivity matrix (rows = subjects, cols = edges)
conmat <- matrix(rnorm(n * p), nrow = n, ncol = p)

# A synthetic covariate (for example age-like nuisance variable)
covariates <- matrix(rnorm(n), ncol = 1)

# Build behavior with both signal and covariate effects
edge_signal <- rowMeans(conmat[, 1:10, drop = FALSE])
behav <- 0.7 * edge_signal + 0.6 * covariates[, 1] + rnorm(n, sd = 0.5)

# Leakage-safe CV call: covariates are handled fold-wise inside resampling
fit <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5
)

collect_metrics(fit)
#> # A tibble: 5 × 5
#>    fold n_assess    both     pos    neg
#>   <int>    <int>   <dbl>   <dbl>  <dbl>
#> 1     1       16  0.142   0.142  NA    
#> 2     2       16  0.447   0.447  NA    
#> 3     3       16  0.163   0.294  -0.270
#> 4     4       16  0.694   0.694  NA    
#> 5     5       16 -0.0624 -0.0624 NA
```

## Current API

Use `fit(cpm_spec(...), ...)` for a single fit and
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
for CV.

``` r
single_fit <- fit(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  return_edges = "sum"
)

resamples_fit <- fit_resamples(
  cpm_spec(),
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5
)

collect_metrics(resamples_fit)
#> # A tibble: 5 × 5
#>    fold n_assess   both    pos     neg
#>   <int>    <int>  <dbl>  <dbl>   <dbl>
#> 1     1       16  0.273  0.361 -0.195 
#> 2     2       16 -0.219 -0.202 -0.0348
#> 3     3       16  0.330  0.330 NA     
#> 4     4       16  0.502  0.502 NA     
#> 5     5       16  0.182  0.182 NA
collect_predictions(resamples_fit)
#> # A tibble: 80 × 6
#>      row  fold     real    both     pos       neg
#>    <int> <int>    <dbl>   <dbl>   <dbl>     <dbl>
#>  1     1     4 -0.586   -0.137  -0.137   1.65e-17
#>  2     2     4  0.726   -0.0626 -0.0626  1.65e-17
#>  3     3     2  0.478    0.112   0.0806  6.62e- 2
#>  4     4     3  0.426    0.147   0.147  -1.73e-17
#>  5     5     5 -1.53    -0.0774 -0.0774 -1.04e-17
#>  6     6     3  0.500    0.568   0.568  -1.73e-17
#>  7     7     5 -0.272   -0.0547 -0.0547 -1.04e-17
#>  8     8     4 -0.00964 -0.238  -0.238   1.65e-17
#>  9     9     5  0.201    0.0220  0.0220 -1.04e-17
#> 10    10     3  0.556    0.150   0.150  -1.73e-17
#> # ℹ 70 more rows
collect_edges(resamples_fit, format = "index")
#> NULL
```

## Edge Storage Tips

For large feature spaces, fold-wise edge storage can grow quickly.

- Use `return_edges = "sum"` to keep only fold-aggregated edge counts.
- Use `collect_edges(..., format = "index")` to export sparse indices.
- Use `return_edges = "none"` if you only need predictive performance.

## Anti-Leakage Checklist

Use this checklist when building CPM workflows:

- Split train/test folds before any parameter-learning step.
- Fit covariate regression on the training split only.
- Apply training-fitted covariate regression to the fold’s test split.
- Keep scaling and feature selection inside each fold.
- If comparing multiple settings, use nested CV for model selection.
- Report the CV design and covariate strategy explicitly.

## References

- Snoek L, Miletic S, Scholte HS (2019). How to control for confounds in
  decoding analyses of neuroimaging data. NeuroImage.
  <https://doi.org/10.1016/j.neuroimage.2018.09.074>
- Scheinost D, Noble S, Horien C, et al. (2019). Ten simple rules for
  predictive modeling of individual differences in neuroimaging.
  NeuroImage. <https://doi.org/10.1016/j.neuroimage.2019.02.057>
- Rosenblatt M, Tejavibulya L, Jiang R, Noble S, Scheinost D (2024).
  Data leakage inflates prediction performance in connectome-based
  machine learning models. Nature Communications.
  <https://doi.org/10.1038/s41467-024-46150-w>
