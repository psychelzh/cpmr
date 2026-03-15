
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpmr

<!-- badges: start -->

[![R-CMD-check](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychelzh/cpmr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/psychelzh/cpmr/graph/badge.svg)](https://app.codecov.io/gh/psychelzh/cpmr)
[![CRAN
status](https://www.r-pkg.org/badges/version/cpmr)](https://CRAN.R-project.org/package=cpmr)
<!-- badges: end -->

`cpmr` brings connectome-based predictive modeling (CPM) into the
tidymodels ecosystem. It provides:

- a `parsnip` model specification through `cpm_reg()`;
- workflow-friendly fitting with formulas or recipes;
- tuning support for CPM thresholds;
- CPM-aware metrics such as `cpm_cor()` and `cpm_spearman()`;
- edge inspection helpers through `collect_edges()`.

The expected predictor layout is one subject per row and one edge per
column.

## Installation

You can install the released version of cpmr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cpmr")
```

Or you can install the development version from GitHub with:

``` r
pak::pak("psychelzh/cpmr")
```

## Fit A CPM Model

``` r
library(cpmr)
library(parsnip)

withr::local_seed(123)

n <- 80
p <- 40
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- x[, 1] - x[, 2] + 0.5 * x[, 3] + rnorm(n, sd = 0.5)

dat <- as.data.frame(x)
names(dat) <- paste0("edge_", seq_len(p))
dat$y <- y

spec <- cpm_reg(
  thresh_method = "sparsity",
  thresh_level = 0.2,
  network = "both"
)

fit_obj <- fit(spec, y ~ ., data = dat[1:60, ])
predict(fit_obj, dat[61:80, ])
#> # A tibble: 20 × 1
#>      .pred
#>      <dbl>
#>  1  0.397 
#>  2 -0.0251
#>  3  0.751 
#>  4  1.06  
#>  5  0.266 
#>  6 -0.398 
#>  7  0.756 
#>  8 -0.453 
#>  9 -0.895 
#> 10 -1.72  
#> 11  0.375 
#> 12 -0.343 
#> 13  1.21  
#> 14  0.127 
#> 15 -0.556 
#> 16 -0.841 
#> 17  0.0704
#> 18 -0.382 
#> 19 -0.890 
#> 20  0.525
collect_edges(fit_obj)[1:12, , drop = FALSE]
#>           pos   neg
#> edge_1   TRUE FALSE
#> edge_2  FALSE  TRUE
#> edge_3   TRUE FALSE
#> edge_4  FALSE  TRUE
#> edge_5  FALSE FALSE
#> edge_6  FALSE  TRUE
#> edge_7  FALSE  TRUE
#> edge_8  FALSE FALSE
#> edge_9  FALSE FALSE
#> edge_10  TRUE FALSE
#> edge_11 FALSE FALSE
#> edge_12  TRUE FALSE
```

## Use With Workflows

``` r
suppressPackageStartupMessages({
  library(recipes)
  library(workflows)
})

wf <- workflow() |>
  add_recipe(
    recipe(y ~ ., data = dat[1:60, ]) |>
      step_normalize(all_predictors())
  ) |>
  add_model(spec)

wf_fit <- fit(wf, data = dat[1:60, ])
predict(wf_fit, dat[61:80, ])
#> # A tibble: 20 × 1
#>      .pred
#>      <dbl>
#>  1  0.397 
#>  2 -0.0251
#>  3  0.751 
#>  4  1.06  
#>  5  0.266 
#>  6 -0.398 
#>  7  0.756 
#>  8 -0.453 
#>  9 -0.895 
#> 10 -1.72  
#> 11  0.375 
#> 12 -0.343 
#> 13  1.21  
#> 14  0.127 
#> 15 -0.556 
#> 16 -0.841 
#> 17  0.0704
#> 18 -0.382 
#> 19 -0.890 
#> 20  0.525
```

## Tune CPM Thresholds

``` r
library(rsample)
library(tune)
library(yardstick)

folds <- vfold_cv(dat[1:60, ], v = 3)

tuned_spec <- cpm_reg(
  thresh_method = "sparsity",
  thresh_level = tune(),
  network = "both"
)

tune_res <- workflow() |>
  add_formula(y ~ .) |>
  add_model(tuned_spec) |>
  tune_grid(
    resamples = folds,
    grid = tibble::tibble(thresh_level = c(0.15, 0.25)),
    metrics = metric_set(rmse, cpm_cor)
  )

collect_metrics(tune_res)
#> # A tibble: 4 × 7
#>   thresh_level .metric .estimator  mean     n std_err .config        
#>          <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>          
#> 1         0.15 cpm_cor standard   0.383     3  0.0434 pre0_mod1_post0
#> 2         0.15 rmse    standard   1.28      3  0.186  pre0_mod1_post0
#> 3         0.25 cpm_cor standard   0.444     3  0.0592 pre0_mod2_post0
#> 4         0.25 rmse    standard   1.30      3  0.125  pre0_mod2_post0
```

## Inspect Edges Across Resamples

To retain fold-wise edge masks during tidymodels resampling, use
`extract_cpm_edges()` through the resampling control object and then
summarize them with `collect_edges()`:

``` r
resample_ctrl <- control_resamples(
  save_pred = TRUE,
  extract = extract_cpm_edges
)

resample_res <- workflow() |>
  add_formula(y ~ .) |>
  add_model(spec) |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(rmse, cpm_cor),
    control = resample_ctrl
  )

collect_edges(resample_res, type = "sum")
#> # A tibble: 28 × 4
#>    predictor   pos   neg n_folds
#>    <chr>     <int> <int>   <int>
#>  1 edge_1        3     0       3
#>  2 edge_10       1     1       3
#>  3 edge_11       0     1       3
#>  4 edge_12       2     0       3
#>  5 edge_14       0     3       3
#>  6 edge_15       3     0       3
#>  7 edge_16       0     1       3
#>  8 edge_17       1     0       3
#>  9 edge_19       1     0       3
#> 10 edge_2        0     3       3
#> # ℹ 18 more rows
```

## Repeated Measures

If rows from the same subject can appear multiple times, keep those rows
together during resampling with `group_vfold_cv()` and exclude the group
identifier from the predictors:

``` r
subject_id <- factor(rep(paste0("subj_", seq_len(30)), each = 2))
grouped_dat <- dat[rep(seq_len(30), each = 2), ]
grouped_dat <- transform(grouped_dat, subject_id = subject_id)

group_folds <- group_vfold_cv(grouped_dat, group = subject_id, v = 3)

group_res <- workflow() |>
  add_formula(y ~ . - subject_id) |>
  add_model(spec) |>
  fit_resamples(
    resamples = group_folds,
    metrics = metric_set(rmse, cpm_cor),
    control = control_resamples(extract = extract_cpm_edges)
  )

collect_metrics(group_res)
#> # A tibble: 2 × 6
#>   .metric .estimator  mean     n std_err .config        
#>   <chr>   <chr>      <dbl> <int>   <dbl> <chr>          
#> 1 cpm_cor standard   0.409     3  0.0446 pre0_mod0_post0
#> 2 rmse    standard   1.37      3  0.114  pre0_mod0_post0
collect_edges(group_res, type = "sum")
#> # A tibble: 26 × 4
#>    predictor   pos   neg n_folds
#>    <chr>     <int> <int>   <int>
#>  1 edge_1        3     0       3
#>  2 edge_10       1     0       3
#>  3 edge_12       2     0       3
#>  4 edge_14       0     1       3
#>  5 edge_15       2     0       3
#>  6 edge_16       0     1       3
#>  7 edge_17       1     0       3
#>  8 edge_2        0     3       3
#>  9 edge_22       3     0       3
#> 10 edge_23       1     0       3
#> # ℹ 16 more rows
```

## Design Notes

CPM is a protocol, not just a single regression call. In `cpmr`, edge
selection, network aggregation, and model fitting are all kept inside
each resampling split so that feature selection stays independent from
assessment data.

## Code Of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
