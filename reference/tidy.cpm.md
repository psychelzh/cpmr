# Tidy a `cpm` object

Tidy a `cpm` object

## Usage

``` r
# S3 method for class 'cpm'
tidy(x, ..., component = c("performance", "edges"))
```

## Arguments

- x:

  A `cpm` object.

- ...:

  Additional arguments passed to
  [`summary()`](https://rdrr.io/r/base/summary.html).

- component:

  A character vector indicating the component to tidy.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with columns storing parameters of the
[`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) object and
further columns depending on the `component` argument:

For `component = "performance"`:

- method:

  The method used to calculate the correlation between the real and
  predicted values.

- pos:

  The correlation between the real and predicted values for positive
  edges.

- neg:

  The correlation between the real and predicted values for negative
  edges.

For `component = "edges"`:

- level:

  The proportional threshold for edge selection.

- pos:

  A logical vector indicating whether each edge is selected based on the
  edge_level (positive).

- neg:

  A logical vector indicating whether each edge is selected based on the
  edge_level (negative).
