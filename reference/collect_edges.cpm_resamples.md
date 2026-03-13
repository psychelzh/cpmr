# Collect selected edges from CPM resamples

Collect selected edges from CPM resamples

## Usage

``` r
# S3 method for class 'cpm_resamples'
collect_edges(x, format = c("raw", "index"), ...)
```

## Arguments

- x:

  A `cpm_resamples` object.

- format:

  Output format for edges. Use `"raw"` to return the stored matrix/array
  directly. Use `"index"` to return sparse edge indices.

- ...:

  For future extension. Currently ignored.

## Value

A matrix for `return_edges = "sum"`, a 3D array for
`return_edges = "all"`, or `NULL` for `return_edges = "none"` when
`format = "raw"`. For `format = "index"`, returns sparse edge indices.
