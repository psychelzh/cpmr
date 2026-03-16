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
with columns storing parameters of the `cpm` object and further columns
depending on the `component` argument:

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

- pos:

  A logical vector indicating whether each edge is selected by the
  fitted CPM model (positive).

- neg:

  A logical vector indicating whether each edge is selected by the
  fitted CPM model (negative).
