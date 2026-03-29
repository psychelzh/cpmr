# Summary of a cpm object.

This function provides a summary of a `cpm` object, including the
prediction performance and the selected edges.

## Usage

``` r
# S3 method for class 'cpm'
summary(object, ..., method = c("pearson", "spearman"), edge_level = 0.5)

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

- edge_level:

  A numeric value between 0 and 1 indicating the proportional threshold
  for edge selection.

- x:

  An object of class `cpm_summary`.

## Value

A list of class `cpm_summary` containing two elements:

- performance:

  A matrix of prediction performance, including the correlation between
  the real and predicted values for both edges, positive edges only, and
  negative edges only.

- edges:

  A logical vector indicating whether each edge is selected based on the
  edge_level.

- params:

  A list of parameters used in the summary.
