# Define CPM strength-construction settings

Build the feature-construction portion of a
[`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md). This
helper configures the current CPM path that turns screened edges into
subject-level strength-style predictors. The name uses "strength"
because this stage follows the CPM tradition of constructing
subject-level scores from screened edge sets, but it is not limited to a
simple raw network-strength sum: positive and negative edges can be
represented separately or collapsed into a net score, optional weighting
can be applied before the score is formed, and edge values can be
standardized within each training set before constructing the final
predictors.

## Usage

``` r
cpm_construction_strength(
  sign_mode = c("separate", "net"),
  weight_scale = 0,
  standardize_edges = FALSE
)
```

## Arguments

- sign_mode:

  How positive and negative screened information is represented.
  `"separate"` constructs `positive_summary` and `negative_summary`,
  fits a `joint` stream from both together, and also returns `positive`
  and `negative` positive-only / negative-only diagnostic streams.
  `"net"` constructs one
  `net_summary = positive_summary - negative_summary` feature and
  returns a single `net` stream.

- weight_scale:

  Non-negative scale controlling optional sigmoid-style edge weighting
  before strength construction. `0` disables additional weighting and
  keeps the classic hard-threshold CPM score. Values greater than `0`
  apply the current sigmoid weighting scheme, with larger values
  producing a smoother transition around the selection cutoff.

- standardize_edges:

  Logical value indicating if edge values should be standardized within
  each training set before CPM feature construction. If `TRUE`, each
  edge is centered and scaled to unit variance using the training data,
  and the same transformation is applied again at prediction time. This
  follows the fold-local edge z-scoring approach described by Rapuano et
  al. (2020). Defaults to `FALSE` so the default construction stays
  close to the classic CPM strength workflow; set it to `TRUE` when you
  want this additional preprocessing step explicitly.

## Examples

``` r
cpm_construction_strength()
#> $type
#> [1] "strength"
#> 
#> $sign_mode
#> [1] "separate"
#> 
#> $weight_scale
#> [1] 0
#> 
#> $standardize_edges
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "cpm_construction_spec"
cpm_construction_strength(
  sign_mode = "net",
  weight_scale = 0.03
)
#> $type
#> [1] "strength"
#> 
#> $sign_mode
#> [1] "net"
#> 
#> $weight_scale
#> [1] 0.03
#> 
#> $standardize_edges
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "cpm_construction_spec"
```
