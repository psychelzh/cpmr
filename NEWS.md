# cpmr (development version)

## Breaking changes

* Removed the legacy `cpm()` entry point and the deprecated `confounds` alias.
  Use `fit(cpm_spec(...), ...)` / `fit_resamples(cpm_spec(...), ...)` for the
  native workflow.
* `fit()` now performs a single fit; resampling is handled by
  `fit_resamples()`.
* `cpm_resamples` no longer stores redundant resample metrics, and the
  `collect_*()` helpers have been removed. Use `summary()` for aggregated
  resample results and `predictions`, `edges`, and `folds` for raw outputs.
* `cpm_spec()` has moved from a flat threshold-only API to a helper-based
  interface with explicit `screen`, `feature_space`, `weighting`, and `model`
  components.
* `bias_correct` has been renamed to `standardize_edges`, and now defaults to
  `FALSE` so the default native workflow stays closer to classic CPM
  network-strength construction.
* Native CPM terminology has been updated: `network_summary` is now
  `feature_space`, the classic multi-strength stream is `joint`, and the
  single signed stream is `net`.

## Enhancements

* Added `cpm_spec()` as the native interface for `fit()` and `fit_resamples()`,
  and made single-fit and resample result objects more consistent.
* Added `cpm_screen()`, `cpm_weighting()`, and `cpm_model_lm()` to make native
  CPM specifications more readable and reusable, and removed the intermediate
  `cpm_threshold()` helper.
* Expanded native CPM screening beyond the previous flat defaults:
  `cpm_screen()` now exposes `rule = "cor_p"`, `"sparsity"`, or `"cor_abs"`,
  while optional `control = list(cor_method = ...)` keeps lower-frequency
  choices like Pearson versus Spearman screening available without crowding the
  default path.
* Added explicit native CPM feature construction choices with
  `feature_space = "separate"` and `feature_space = "net"`.
* Added weighted CPM feature construction with `cpm_weighting("binary")` and
  `cpm_weighting("sigmoid")`.
* Added `summary.cpm_resamples()`, which now reports pooled out-of-fold error
  metrics by default and keeps pooled / fold-wise correlations as supplementary
  statistics.
* Added `resample_metrics()` for direct access to pooled or fold-wise metric
  tables from a `cpm_resamples` object.
* Added native-first documentation, including a getting-started vignette and
  reorganized pkgdown reference pages.

## Maintenance

* Refactored internals and expanded test coverage around fit, summary, edge,
  and resampling behavior.
* Hardened resample validation so each fold must retain at least 3
  complete-case training observations.
* Applied `sparsity` thresholding per sign, rather than reusing one absolute
  cutoff across both tails.
* Fixed the `joint` linear-model path when one CPM strength is aliased or
  empty.

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
