---
name: cpmr-r-development
description: Work on the cpmr R package in this repository. Use when editing cpmr package code, tests, or documentation, especially around the staged selection/construction/model architecture, AIR-based formatting, devtools workflows, and testthat coverage.
---

# CPMR R Development

Use this skill for changes inside the `cpmr` package repo.
Read the package surface before editing: start with `README.Rmd`, then open the
relevant files in `R/` and matching tests in `tests/testthat/`.

## Project Workflow

1. Understand the affected stage before changing code.
   In this repo, keep the public CPM stages visible as `selection`,
   `construction`, and `model`.
2. Preserve the staged API.
   `cpm_spec()` should stay readable at the top level, while stage-specific
   helpers carry their own arguments.
3. Keep runtime paths straight.
   Avoid helper layers that only forward arguments, avoid repeated
   validation/normalization in internal spec-based paths, and prefer computing a
   value once over recomputing it through wrappers.
4. Keep the `model` layer extensible.
   Even if `lm` is the only current model, preserve a clear model boundary so
   future model types have an obvious integration point.
5. Match tests to the layer you changed.
   Update the closest `testthat` file first, then run broader tests when the
   change crosses multiple stages.

## Repo-Specific Guidance

- `R/cpm-spec*.R`: public staged spec helpers and validation
- `R/fit-selection.R`: edge-selection runtime
- `R/fit-construction.R`: feature construction and cached summaries
- `R/fit-pipeline.R`: train/test flow, construction, model fit, prediction
- `R/fit-runner.R`: top-level fit orchestration
- `tests/testthat/`: stage-aligned tests

When simplifying code in this repo:

- Prefer one real runtime function over `foo()` plus `foo_impl()` when there is
  only one meaningful path.
- Put validation and normalization at public constructors or convenience
  wrappers, not deep in internal runtime helpers that already receive parsed
  specs.
- Preserve training-time caches when prediction reuses training data.
- Keep names literal and stage-based instead of introducing abstract temporary
  state.

## Quality Checks

Use AIR for formatting.
This repo already points VS Code at `Posit.air-vscode` and CI uses
`posit-dev/setup-air`.

```bash
air format .
air format R/file.R
```

After R code changes, run:

```r
devtools::document()
devtools::test()
```

For narrower iteration loops, use:

```r
devtools::test(filter = "fit-model")
testthat::test_file("tests/testthat/test-fit-model.R")
```

If roxygen examples, README examples, or pkgdown-facing docs changed, update the
generated outputs that belong with those sources before finishing.
