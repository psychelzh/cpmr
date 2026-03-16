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

summary(fit)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: 0.277 (SE 0.132)
#>     Positive: 0.303 (SE 0.129)
#>     Negative: -0.270 (SE NA)
```

## Native API

``` r
spec <- cpm_spec()

single_fit <- fit(
  spec,
  conmat = conmat,
  behav = behav,
  covariates = covariates
)

resamples_fit <- fit_resamples(
  spec,
  conmat = conmat,
  behav = behav,
  covariates = covariates,
  kfolds = 5
)

summary(resamples_fit)
#> CPM resample summary:
#>   Number of folds: 5
#>   Performance:
#>     Combined: 0.214 (SE 0.120)
#>     Positive: 0.235 (SE 0.120)
#>     Negative: -0.115 (SE 0.080)
head(resamples_fit$predictions)
#>   row fold       real        both         pos           neg
#> 1   1    4 -0.5862850 -0.13713172 -0.13713172  1.647987e-17
#> 2   2    4  0.7262082 -0.06258281 -0.06258281  1.647987e-17
#> 3   3    2  0.4776754  0.11181262  0.08055321  6.622293e-02
#> 4   4    3  0.4263198  0.14677926  0.14677926 -1.734723e-17
#> 5   5    5 -1.5329525 -0.07744804 -0.07744804 -1.040834e-17
#> 6   6    3  0.4996146  0.56813333  0.56813333 -1.734723e-17
dim(resamples_fit$edges)
#> NULL
```

If you prefer explicit parameter objects, the lower-level
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md) +
[`fit()`](https://generics.r-lib.org/reference/fit.html) /
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
path is the native interface and routes through the same CPM core.

## Edge Storage Tips

For large feature spaces, fold-wise edge storage can grow quickly.

- Use `return_edges = "sum"` to keep only fold-aggregated edge counts.
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
