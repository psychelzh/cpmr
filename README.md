
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
#>         pos   neg
#>  [1,]  TRUE FALSE
#>  [2,] FALSE  TRUE
#>  [3,]  TRUE FALSE
#>  [4,] FALSE  TRUE
#>  [5,] FALSE FALSE
#>  [6,] FALSE  TRUE
#>  [7,] FALSE  TRUE
#>  [8,] FALSE FALSE
#>  [9,] FALSE FALSE
#> [10,]  TRUE FALSE
#> [11,] FALSE FALSE
#> [12,]  TRUE FALSE
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

## Design Notes

CPM is a protocol, not just a single regression call. In `cpmr`, edge
selection, network aggregation, and model fitting are all kept inside
each resampling split so that feature selection stays independent from
assessment data.

## Code Of Conduct

Please note that the cpmr project is released with a [Contributor Code
of Conduct](https://psychelzh.github.io/cpmr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
