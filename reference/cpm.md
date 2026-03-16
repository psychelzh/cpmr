# cpm Fitted Object

A `cpm` object is returned by
[`fit()`](https://generics.r-lib.org/reference/fit.html) when fitting a
[`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
on a single dataset.

## Structure

A `cpm` object is a list with the following elements:

- `call`:

  Matched call used for fitting.

- `spec`:

  The originating `cpm_spec` object.

- `params`:

  Parameter list used at fit time.

- `predictions`:

  Data frame of observation-level outputs with columns `row`, `real`,
  `both`, `pos`, and `neg`.

- `edges`:

  Stored single-fit edge mask as a `p x 2` logical matrix with `pos` and
  `neg` columns.

- `model`:

  Trained CPM model components used by prediction.

## See also

[`fit()`](https://generics.r-lib.org/reference/fit.html),
[`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md),
[`tidy.cpm()`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md)
