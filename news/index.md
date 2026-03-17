# Changelog

## cpmr (development version)

### Breaking changes

- Removed the legacy
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) entry
  point and the deprecated `confounds` alias. Use
  `fit(cpm_spec(...), ...)` / `fit_resamples(cpm_spec(...), ...)` for
  the native workflow.
- [`fit()`](https://generics.r-lib.org/reference/fit.html) now performs
  a single fit; resampling is handled by
  [`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md).
- `cpm_resamples` no longer stores redundant resample metrics, and the
  `collect_*()` helpers have been removed. Use
  [`summary()`](https://rdrr.io/r/base/summary.html) for aggregated
  resample results and `predictions`, `edges`, and `folds` for raw
  outputs.

### Enhancements

- Added
  [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  as the native interface for
  [`fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md),
  and made single-fit and resample result objects more consistent.
- Added
  [`summary.cpm_resamples()`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md),
  which now reports pooled out-of-fold error metrics by default and
  keeps pooled / fold-wise correlations as supplementary statistics.
- Added
  [`resample_metrics()`](https://psychelzh.github.io/cpmr/reference/resample_metrics.md)
  for direct access to pooled or fold-wise metric tables from a
  `cpm_resamples` object.
- Added native-first documentation, including a getting-started vignette
  and reorganized pkgdown reference pages.

### Maintenance

- Refactored internals and expanded test coverage around fit, summary,
  edge, and resampling behavior.
- Hardened resample validation so each fold must retain at least 3
  complete-case training observations.

## cpmr 0.1.1

CRAN release: 2026-03-11

### Enhancements

- Renamed internal constant `networks` and `includes` to `corr_types`
  and `inc_edges`, respectively, to better reflect their purpose. In
  addition, the documentation which mentioned `networks` has been
  updated to use correlation types to better reflect their meaning.
- Reorganized `R/cpm.R` so
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) remains
  the entry-point function at the top, followed by `print.cpm()`, with
  internal helpers grouped below.
- Improved test architecture with explicit internal-helper coverage and
  complete-case fold invariants.

### Maintenance

- Refactored the internal
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) workflow
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
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) function
  to handle missing values in the input data
  ([\#2](https://github.com/psychelzh/cpmr/issues/2)).

### Enhancements

- Added `params` to
  [`cpm()`](https://psychelzh.github.io/cpmr/reference/cpm.md) output to
  store the input arguments
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
