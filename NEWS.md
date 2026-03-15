# cpmr (development version)

## Breaking changes

* Rebuilt the package around a tidymodels-first API.
* Removed the legacy `cpm()`, `cpm_spec()`, and `fit_resamples()`
  interfaces.
* The main entry point is now `cpm_reg()` with `fit()`, `predict()`,
  `workflows`, and `tune`.

## Enhancements

* Added the `parsnip` model specification `cpm_reg()` with a native
  `cpmr` engine.
* Added workflow integration for formula- and recipe-based
  preprocessing.
* Added tuning support for CPM thresholds through `tune_grid()`.
* Added CPM-aware metrics `cpm_cor()` and `cpm_spearman()`.
* Added `collect_edges()` methods for fitted CPM engines, parsnip model
  fits, and workflows.
* Added dials parameter helpers for threshold method, threshold level,
  bias correction, and network selection.

## Maintenance

* Reorganized the codebase around RFC-aligned core modules for input
  validation, preprocessing, edge selection, train/predict, and
  resampling.
* Added a minimal tidymodels adapter layer covering `parsnip`,
  `workflows`, `tune`, `yardstick`, and edge collection.
* Replaced the legacy test suite with focused tests for the new core
  engine and tidymodels integration paths.
* Moved architecture planning documents under `design/rfc/` and
  excluded them from package builds.

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
