# cpmr (development version)

## Breaking changes

* **`cpm()` removed.** The legacy entry-point function `cpm()` has been
  deleted. Use `fit(cpm_spec(...), conmat, behav)` instead. The `confounds`
  parameter (deprecated alias of `covariates`) is also removed.
* `fit()` now performs a single fit. Cross-validation/resampling workflows are
  handled by `fit_resamples()`.

## Enhancements

* Added `cpm_spec()` model specification object and `fit()` dispatch as the
  primary API for connectome-based predictive modeling.
* `print.cpm_spec()` provides a concise summary of all modeling parameters.
* Added the `cpm_resamples` result class with dedicated methods:
  `collect_metrics()`, `collect_predictions()`, and `collect_edges()`.
* Added edge export support via `collect_edges(format = "index")` for sparse
  index output when edge storage is large.

## Maintenance

* Refactored fit internals into focused modules (`fit-generics`, `cpm-spec`,
  `cpm-resamples`, and `fit-internals`) to improve maintainability.
* Updated and expanded tests for internal helper branches and edge/summary
  coverage paths.
* Stabilized GitHub Actions uploads for Codecov coverage and Test Analytics by generating and uploading JUnit test results from an explicit workspace path.
* Added architecture RFC documents under `design/rfc/`:
  - `0001-core-engine.md` (core engine stabilization),
  - `0002-tidymodels-adapter.md` (tidymodels integration design),
  - `0003-migration-and-deprecation.md` (phased migration plan).
* Started phase-A core refactor by introducing internal `core_*` helpers and
  routing `fit.cpm_spec()` / `fit_resamples.cpm_spec()` through the core layer.
* Added dedicated core-engine contract tests for single-fit parity, train/test
  covariate handling, and insufficient-complete-case error paths.
* Hardened resample validation so each fold must retain at least 3
  complete-case training observations.
* Moved training/prediction primitives into the core layer and kept
  `fit-internals.R` helpers as compatibility wrappers during phase A.

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
