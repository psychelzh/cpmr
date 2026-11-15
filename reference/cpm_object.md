# cpm object

A `cpm` object is the primary result returned by
[`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md).

## Structure

A `cpm` object is a list with the following elements:

- `call`:

  Matched call used for the CPM run.

- `spec`:

  The originating CPM specification object.

- `settings`:

  Runtime settings used for the CPM run, including missing-data handling
  and edge-storage preferences.

- `predictions`:

  Data frame of observation-level predictions with fold IDs and one
  column per configured prediction stream.

- `edges`:

  Stored edge output based on `return_edges` (`NULL`/matrix/array).

- `folds`:

  List of assessment-row indices for each fold.

## See also

[`cpm()`](https://psychelzh.github.io/cpmr/reference/spec.md),
[`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md),
[`tidy.cpm()`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md)
