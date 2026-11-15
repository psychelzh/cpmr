# Define the outcome model used after CPM feature construction

Build the second-stage model used after CPM has converted selected edges
into subject-level predictors. `cpm_model_lm()` fits an
intercept-inclusive linear regression with the CPM-derived features for
each prediction stream.

## Usage

``` r
cpm_model_lm()
```

## Examples

``` r
cpm_model_lm()
#> $type
#> [1] "lm"
#> 
#> attr(,"class")
#> [1] "cpm_model_spec"
```
