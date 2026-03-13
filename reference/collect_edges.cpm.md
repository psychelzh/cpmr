# Collect selected edges from a CPM fit

Collect selected edges from a CPM fit

## Usage

``` r
# S3 method for class 'cpm'
collect_edges(x, ...)
```

## Arguments

- x:

  A `cpm` object.

- ...:

  For future extension. Currently ignored.

## Value

A matrix for `return_edges = "sum"`, a 3D array for
`return_edges = "all"`, or `NULL` for `return_edges = "none"`.
