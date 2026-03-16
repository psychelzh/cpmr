# cpmr (development version)

## Breaking changes

* Removed the legacy `cpm()` entry point and the deprecated `confounds` alias.
  Use `cpm_fit()` / `cpm_fit_resamples()` for the direct native workflow, or
  `fit(cpm_spec(...), ...)` / `fit_resamples(cpm_spec(...), ...)` for the
  lower-level native interface.
* `fit()` now performs a single fit; resampling is handled by
  `cpm_fit_resamples()` or `fit_resamples()`.
* `cpm_resamples` no longer stores redundant resample metrics, and the
  `collect_*()` helpers have been removed. Use `summary()` for aggregated
  resample results and `predictions`, `edges`, and `folds` for raw outputs.

## Enhancements

* Added native helper entry points `cpm_fit()` and `cpm_fit_resamples()`.
* Added `cpm_spec()` as the lower-level native interface for `fit()` and
  `fit_resamples()`.
* Added `summary.cpm_resamples()` and made single-fit and resample result
  objects more consistent.
* Added native-first documentation, including a getting-started vignette and
  reorganized pkgdown reference pages.

## Maintenance

* Refactored internals and expanded test coverage around fit, summary, edge,
  and resampling behavior.
* Hardened resample validation so each fold must retain at least 3
  complete-case training observations.

# cpmr 0.1.1

## Enhancements

* Renamed internal constant `networks` and `includes` to `corr_types` and `inc_edges`, respectively, to better reflect their purpose. In addition, the documentation which mentioned `networks` has been updated to use correlation types to better reflect their meaning.
* Reorganized `R/cpm.R` so `cpm()` remains the entry-point function at the top, followed by `print.cpm()`, with internal helpers grouped below.
* Improved test architecture with explicit internal-helper coverage and complete-case fold invariants.

## Maintenance

* Refactored the internal `cpm()` workflow into smaller helpers (`normalize_inputs`, `resolve_include_cases`, `apply_confounds_regression`, etc.) while preserving user-facing behavior.
* Removed the obsolete internal alias `regress_counfounds()`.
* Removed broad `Rfast` namespace import in favor of explicit `Rfast::` calls.
* Updated `summary.cpm()` edge summarization path to avoid mutating local object fields.

# cpmr 0.1.0

## New features

* Added `summary()` method to summarize the results of the CPM analysis (#8).
* Added `tidy()` method to tidy the results of the CPM analysis (#10).
* Support `na_action` argument in `cpm()` function to handle missing values in the input data (#2).

## Enhancements

* Added `params` to `cpm()` output to store the input arguments (#14).
* Let `"sum"` be the default value for `return_edges` argument.
* Let the first two dimensions of `edges` in the output be edges and networks, respectively.
* Polish the print method of the `cpm` class.

# cpmr 0.0.9

## New features

* Added support for row/column matrix as input for behavior and confounds data.

## Maintenance

* Added more data checks to ensure the input data are in the correct format.

# cpmr 0.0.8

* Added `return_edges` argument to optionally set how to return edges in the output.

# cpmr 0.0.7

* Convert back to older version of confounds treating.

# cpmr 0.0.6

* Ensure confounds regression are now only used in feature selection.

# cpmr 0.0.5

* Fixed confounds treatment. Now confounds are used in feature selection but not in model fitting.

# cpmr 0.0.4

* Ensure sparsity threshold method work as expect.
* Some other improvements in code quality.

# cpmr 0.0.3

* Keep observation names in the output.
* Check if observation names match between neural data and behavioral data.

# cpmr 0.0.2

* Added support for confounding variables.

# cpmr 0.0.1

* Initial commit to r-universe.
