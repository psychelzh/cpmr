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

- `model`:

  Trained CPM model components used by prediction.

- `real`:

  Observed behavior values after preprocessing decisions.

- `pred`:

  Matrix of in-sample predictions (`both`, `pos`, `neg`).

- `edges`:

  Stored edge selection output according to `return_edges`.

- `folds`:

  List of row indices used for fitting. For single-fit, this contains
  one element with all included cases.

- `params`:

  Parameter list used at fit time.

## See also

[`fit()`](https://generics.r-lib.org/reference/fit.html),
[`summary.cpm()`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md),
[`tidy.cpm()`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md),
[`collect_edges()`](https://psychelzh.github.io/cpmr/reference/collect_edges.md)
