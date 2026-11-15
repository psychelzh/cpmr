# Changelog

## cpmr (development version)

### Breaking changes

- [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) now uses
  a staged specification created by
  [`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md). The
  old `thresh_method`, `thresh_level`, `kfolds`, and `bias_correct`
  arguments have been replaced by `spec`, `resamples`, and staged helper
  constructors. Unused arguments passed through `...` now cause an error
  rather than being ignored.
- Removed the deprecated `confounds` alias. Use `covariates` instead.
- The returned `cpm` object now stores workflow state under `spec`,
  `settings`, `predictions`, and `folds`. Code that accessed the old
  `real`, `pred`, or `params` fields must be updated.

### Enhancements

- Added [`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md)
  together with
  [`cpm_selection_cor()`](https://psychelzh.github.io/cpmr/reference/cpm_selection_cor.md),
  [`cpm_construction_strength()`](https://psychelzh.github.io/cpmr/reference/cpm_construction_strength.md),
  and
  [`cpm_model_lm()`](https://psychelzh.github.io/cpmr/reference/cpm_model_lm.md)
  to make screening, summarization, and outcome-model choices explicit.
- [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) now
  accepts integer or manual assessment plans through `resamples`, while
  keeping leave-one-out assessment as the default when `resamples` is
  `NULL`.
- [`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md)
  and
  [`tidy.cpm()`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md)
  now focus on correlation-based reporting, with pooled and fold-wise
  correlation tables plus optional edge summaries.
- Unweighted edge standardization now processes only selected edges when
  constructing training and assessment features, reducing time and
  memory use for sparse edge selections.
- Fold execution discards intermediate model state after each fit
  instead of retaining it until all predictions have been assembled.
- Fold-wise edge storage avoids copying the full edge array for every
  fold.
- Reworked package documentation and pkgdown navigation around the
  [`spec()`](https://psychelzh.github.io/cpmr/reference/spec.md) +
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md)
  workflow, including a native getting-started vignette.

### Maintenance

- Corrected covariate-adjusted p-value screening degrees of freedom and
  predictions from rank-deficient nuisance regressions.
- Simplified the internal CPM workflow around fold-based execution and
  aligned `tests/testthat` with the current `R/` modules.
- Hardened assessment-fold validation and leakage-safe preprocessing so
  each fold must retain at least 3 complete-case training observations.

## cpmr 0.1.1

CRAN release: 2026-03-11

### Enhancements

- Renamed internal constant `networks` and `includes` to `corr_types`
  and `inc_edges`, respectively, to better reflect their purpose. In
  addition, the documentation which mentioned `networks` has been
  updated to use correlation types to better reflect their meaning.
- Reorganized `R/cpm.R` so
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) remains
  the entry-point function at the top, followed by `print.cpm()`, with
  internal helpers grouped below.
- Improved test architecture with explicit internal-helper coverage and
  complete-case fold invariants.

### Maintenance

- Refactored the internal
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) workflow
  into smaller helpers (`normalize_inputs`, `resolve_include_cases`,
  `apply_confounds_regression`, etc.) while preserving user-facing
  behavior.
- Removed the obsolete internal alias `regress_counfounds()`.
- Removed broad `Rfast` namespace import in favor of explicit `Rfast::`
  calls.
- Updated
  [`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md)
  edge summarization path to avoid mutating local object fields.

## cpmr 0.1.0

CRAN release: 2024-10-06

### New features

- Added [`summary()`](https://rdrr.io/r/base/summary.html) method to
  summarize the results of the CPM analysis
  ([\#8](https://github.com/psychelzh/cpmr/issues/8)).
- Added [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  method to tidy the results of the CPM analysis
  ([\#10](https://github.com/psychelzh/cpmr/issues/10)).
- Support `na_action` argument in
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) function
  to handle missing values in the input data
  ([\#2](https://github.com/psychelzh/cpmr/issues/2)).

### Enhancements

- Added `params` to
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md) output
  to store the input arguments
  ([\#14](https://github.com/psychelzh/cpmr/issues/14)).
- Let `"sum"` be the default value for `return_edges` argument.
- Let the first two dimensions of `edges` in the output be edges and
  networks, respectively.
- Polish the print method of the `cpm` class.

## cpmr 0.0.9

CRAN release: 2024-06-08

### New features

- Added support for row/column matrix as input for behavior and
  confounds data.

### Maintenance

- Added more data checks to ensure the input data are in the correct
  format.

## cpmr 0.0.8

CRAN release: 2024-03-14

- Added `return_edges` argument to optionally set how to return edges in
  the output.

## cpmr 0.0.7

- Convert back to older version of confounds treating.

## cpmr 0.0.6

- Ensure confounds regression are now only used in feature selection.

## cpmr 0.0.5

- Fixed confounds treatment. Now confounds are used in feature selection
  but not in model fitting.

## cpmr 0.0.4

- Ensure sparsity threshold method work as expect.
- Some other improvements in code quality.

## cpmr 0.0.3

- Keep observation names in the output.
- Check if observation names match between neural data and behavioral
  data.

## cpmr 0.0.2

- Added support for confounding variables.

## cpmr 0.0.1

- Initial commit to r-universe.
