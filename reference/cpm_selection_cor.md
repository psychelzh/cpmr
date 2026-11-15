# Define CPM correlation-based edge-selection settings

Build the selection portion of a
[`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md). This
helper configures the current correlation-based edge-selection path by
choosing the correlation method used to relate each edge to the outcome
and how the associated `level` is interpreted.

## Usage

``` r
cpm_selection_cor(
  method = c("pearson", "spearman"),
  criterion = c("p_value", "absolute", "proportion"),
  level = 0.01
)
```

## Arguments

- method:

  Correlation method used during edge selection. `"pearson"` is the
  default CPM path; `"spearman"` can be useful when a monotonic but
  non-linear relationship is expected.

- criterion:

  How `level` is interpreted. `"p_value"` thresholds the absolute
  correlation by the critical value implied by `level`. When covariates
  are used, the test accounts for the effective training-set covariate
  rank. Spearman screening uses the same t approximation after
  residualization. `"absolute"` treats `level` as a direct
  absolute-correlation cutoff. `"proportion"` retains a per-sign
  proportion of positive and negative edges separately. Because edge
  counts are discrete and ties at the cutoff are retained, the realized
  proportion may be slightly larger than the requested `level`.

- level:

  Numeric level associated with `criterion`.

## Examples

``` r
cpm_selection_cor()
#> $type
#> [1] "cor"
#> 
#> $method
#> [1] "pearson"
#> 
#> $criterion
#> [1] "p_value"
#> 
#> $level
#> [1] 0.01
#> 
#> attr(,"class")
#> [1] "cpm_selection_spec"
cpm_selection_cor(method = "spearman", criterion = "absolute", level = 0.1)
#> $type
#> [1] "cor"
#> 
#> $method
#> [1] "spearman"
#> 
#> $criterion
#> [1] "absolute"
#> 
#> $level
#> [1] 0.1
#> 
#> attr(,"class")
#> [1] "cpm_selection_spec"
```
