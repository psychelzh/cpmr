# Collect fold-level metrics from CPM resamples

Collect fold-level metrics from CPM resamples

## Usage

``` r
# S3 method for class 'cpm_resamples'
collect_metrics(x, ...)
```

## Arguments

- x:

  A `cpm_resamples` object.

- ...:

  For future extension. Currently ignored.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per fold.
