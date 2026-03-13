# Collect fold-level metrics from resampling results

Generic for extracting fold-level metrics from resampling result
objects.

## Usage

``` r
collect_metrics(x, ...)
```

## Arguments

- x:

  A resampling result object.

- ...:

  Additional arguments passed to method implementations.

## Value

A data frame or tibble with one row per fold.
