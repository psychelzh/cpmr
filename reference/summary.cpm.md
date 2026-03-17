# Summary of a cpm object.

This function provides a summary of a `cpm` object, including the
prediction performance and the selected edges.

## Usage

``` r
# S3 method for class 'cpm'
summary(object, ..., method = c("pearson", "spearman"))

# S3 method for class 'cpm_summary'
print(x, ...)
```

## Arguments

- object:

  An object of class `cpm`.

- ...:

  Other parameters passed to the function.

- method:

  A character vector indicating the method used to calculate the
  correlation between the real and predicted values.

- x:

  An object of class `cpm_summary`.

## Value

A list of class `cpm_summary` containing:

- metrics:

  A data frame with columns `level`, `metric`, `prediction`, `estimate`,
  `std_error`, and `method`. Single-fit CPM summaries store correlation
  metrics at `level = "single"`.

- edges:

  A logical matrix indicating which edges are selected by the fitted CPM
  model.

- params:

  A list of parameters used in the summary.
