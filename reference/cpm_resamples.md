# cpm_resamples Resampling Object

A `cpm_resamples` object is returned by
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
and stores fold-level outputs from resampling.

## Structure

A `cpm_resamples` object is a list with the following elements:

- `spec`:

  The originating `cpm_spec` object.

- `folds`:

  List of assessment-row indices for each fold.

- `metrics`:

  Data frame of fold-level performance metrics.

- `predictions`:

  Data frame of observation-level predictions with fold IDs.

- `edges`:

  Stored edge output based on `return_edges` (`NULL`/matrix/array).

- `params`:

  Parameter list used for the resampling run.

## See also

[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md),
[`collect_metrics()`](https://psychelzh.github.io/cpmr/reference/collect_metrics.md),
[`collect_predictions()`](https://psychelzh.github.io/cpmr/reference/collect_predictions.md),
[`collect_edges()`](https://psychelzh.github.io/cpmr/reference/collect_edges.md)
