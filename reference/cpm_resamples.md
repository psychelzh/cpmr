# cpm_resamples Resampling Object

A `cpm_resamples` object is returned by
[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
and stores observation-level predictions together with the resampling
structure that produced them.

## Structure

A `cpm_resamples` object is a list with the following elements:

- `call`:

  Matched call used for resampling.

- `spec`:

  The originating `cpm_spec` object.

- `params`:

  Parameter list used for the resampling run.

- `predictions`:

  Data frame of observation-level predictions with fold IDs.

- `edges`:

  Stored edge output based on `return_edges` (`NULL`/matrix/array).

- `folds`:

  List of assessment-row indices for each fold.

## See also

[`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md),
[`summary.cpm_resamples()`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md)
