# Tidy a `cpm` object

Tidy a `cpm` object

## Usage

``` r
# S3 method for class 'cpm'
tidy(
  x,
  ...,
  component = c("metrics", "edges"),
  level = c("foldwise", "pooled")
)
```

## Arguments

- x:

  A `cpm` object.

- ...:

  Additional arguments passed to
  [`summary()`](https://rdrr.io/r/base/summary.html).

- component:

  A character vector indicating the component to tidy. Use `"metrics"`
  for correlation tables derived from
  [`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md),
  or `"edges"` for stored edge output.

- level:

  Correlation table to return when `component = "metrics"`. `"pooled"`
  returns pooled out-of-fold correlations, and `"foldwise"` returns one
  row per fold and prediction stream.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with component-specific columns.
