# Package index

## Native Fit API

Primary entry points for fitting CPM models natively.

- [`cpm_spec()`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  [`fit(`*`<cpm_spec>`*`)`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  [`fit_resamples(`*`<cpm_spec>`*`)`](https://psychelzh.github.io/cpmr/reference/cpm_spec.md)
  : Define a CPM model specification
- [`fit_resamples()`](https://psychelzh.github.io/cpmr/reference/fit_resamples.md)
  : Fit a model specification on resamples

## Result Objects

S3 result classes returned by single-fit and resampling workflows.

- [`cpm`](https://psychelzh.github.io/cpmr/reference/cpm.md) : cpm
  Fitted Object
- [`cpm_resamples`](https://psychelzh.github.io/cpmr/reference/cpm_resamples.md)
  : cpm_resamples Resampling Object

## Inspect Results

Summary and tidying helpers for fitted results.

- [`resample_metrics()`](https://psychelzh.github.io/cpmr/reference/resample_metrics.md)
  :

  Extract resampling metrics from a `cpm_resamples` object

- [`summary(`*`<cpm>`*`)`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md)
  [`print(`*`<cpm_summary>`*`)`](https://psychelzh.github.io/cpmr/reference/summary.cpm.md)
  : Summary of a cpm object.

- [`summary(`*`<cpm_resamples>`*`)`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md)
  [`print(`*`<cpm_resamples_summary>`*`)`](https://psychelzh.github.io/cpmr/reference/summary.cpm_resamples.md)
  :

  Summarize a `cpm_resamples` object

- [`tidy(`*`<cpm>`*`)`](https://psychelzh.github.io/cpmr/reference/tidy.cpm.md)
  :

  Tidy a `cpm` object

## Package

- [`cpmr`](https://psychelzh.github.io/cpmr/reference/cpmr-package.md)
  [`cpmr-package`](https://psychelzh.github.io/cpmr/reference/cpmr-package.md)
  : cpmr: Connectome Predictive Modelling in R
