---
name: cpmr-r-development
description: Use when working in the cpmr repository to preserve its spec()/cpm() API, staged CPM configuration, direct internal flow, AIR formatting, pkgdown site checks, full test coverage, and final devtools::check() validation.
---

# cpmr R Development

Use this skill for code, tests, docs, review, and release-quality validation in this repository.

## API and design rules

- Public entry points are `spec()` and `cpm()`. Do not reintroduce `fit()`, `fit_resamples()`, `cpm_spec()`, or `cpm_resamples()`.
- Keep CPM model definition separate from execution settings.
  `spec()` holds staged CPM choices; `cpm()` holds runtime settings such as `resamples`, `covariates`, `return_edges`, and `na_action`.
- Keep validation and normalization at public constructors and workflow entry points. Internal runtime paths should trust staged objects instead of re-validating them.
- Prefer direct, stepwise code. Avoid extra helper layers unless they remove real duplication or clarify a stable boundary.
- Private helpers should usually come after their primary callers so the main workflow reads top-down.
- Keep `tests/testthat` aligned with the current `R/` module boundaries. When `R/` files are merged, renamed, or deleted, rename or regroup the corresponding tests so the test layout does not preserve outdated file or concept names.

## CPM architecture

- Treat CPM as a staged workflow: screen edges, summarize selected signal, then fit the outcome model.
- The default user workflow is fold-based through `cpm()`.
- Keep train-only computations leakage-safe. Anything learned from training data and reused at assessment time belongs in the fitted runtime state, not in public configuration.

## Code quality flow

Run this sequence after meaningful changes:

1. `devtools::document()`
2. `devtools::test()`
3. `pkgdown::build_site(preview = FALSE, install = TRUE)`
4. `cov <- covr::package_coverage()`
5. `covr::percent_coverage(cov)` must be `100`
6. If coverage is short, inspect `covr::zero_coverage(cov)` and close the gap
7. `devtools::check()`

## Tooling notes

- Format with `air`, for example `air format .` or `air format R/file.R`.
- Because this repository tracks pkgdown configuration, update `_pkgdown.yml` when exported topics or public narratives change.
- On Windows, use Quarto or the RStudio-bundled Pandoc if `devtools::check()` or `pkgdown` needs Pandoc.
- Build the pkgdown site with installation enabled when vignettes call `library(cpmr)`.
