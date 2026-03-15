# RFC 0001: CPM Core Engine Stabilization

- Status: Implemented
- Authors: cpmr maintainers
- Target release: 0.2.x
- Last updated: 2026-03-15

## Summary

This RFC defines the stable internal "core engine" layer for `cpmr`. The core
layer is independent from tidymodels orchestration and owns:

1. Input validation and normalization
2. Leakage-safe training/assessment preprocessing
3. Edge selection
4. Model training and prediction
5. Resample orchestration primitives

The purpose is to keep CPM correctness and leakage-safety in one place, so the
tidymodels surface can remain a thin adapter instead of duplicating modeling
logic.

## Motivation

CPM is a protocol, not a single off-the-shelf model call. To expose it cleanly
through parsnip, workflows, and tune, `cpmr` needs a stable internal layer for
input checks, fold-local preprocessing, edge selection, and prediction.

## Goals

1. Define clear, stable internal function contracts.
2. Keep anti-leakage behavior explicit and hard to accidentally break.
3. Keep the core layer independent from tidymodels packages.
4. Make the adapter layer call the core layer instead of re-implementing CPM.

## Non-goals

1. Adding new modeling families beyond standard CPM regression.
2. Moving preprocessing logic into generic recipe steps when that would weaken
   leakage guarantees.
3. Forcing tidymodels dependencies in the core layer.

## Design Principles

1. Anti-leakage first: all parameter-learning steps remain inside training
   splits.
2. Deterministic contracts: every core helper documents expected shape/types.
3. Minimal coupling: core engine does not import tidymodels packages.
4. Lean internals: the core layer should not carry workflow-framework
   abstractions.

## Proposed Internal Modules

The following files define the target internal organization:

1. `R/core-input.R`
2. `R/core-preprocess.R`
3. `R/core-select-edges.R`
4. `R/core-train-predict.R`
5. `R/core-resample-runner.R`
6. `R/core-objects.R`
These files now exist and serve as the internal CPM engine used by the
tidymodels adapter.

## Internal Contracts

### 1) Input normalization

- `core_normalize_inputs(conmat, behav, covariates = NULL)`
- Returns a list with normalized `behav` vector and optional covariate matrix.
- Errors on row-count mismatch or invalid behavior shape/type.

### 2) Include-case resolution

- `core_resolve_include_cases(conmat, behav, covariates, na_action)`
- `na_action` in `c("fail", "exclude")`.
- Returns integer row indices eligible for fitting/resampling.

### 3) Leakage-safe preprocessing

- `core_prepare_training_data(conmat, behav, covariates, rows_train)`
- `core_prepare_assessment_data(conmat, behav, covariates, rows_train, rows_test, covariates_train = NULL)`
- If covariates are present, nuisance regression is fit on training rows only.

### 4) Edge selection

- `core_select_edges(conmat_train, behav_train, method, level)`
- Returns logical matrix `n_edges x 2` with columns `pos` and `neg`.

### 5) Training and prediction

- `core_train_model(conmat_train, behav_train, edges, bias_correct)`
- Returns trained components required for prediction.
- `core_predict_model(model, conmat_new)` returns matrix with columns:
  `both`, `pos`, `neg`.

### 6) Resample runner

- `core_fit_xy(conmat, behav, thresh_method, thresh_level, bias_correct, network)`
- `core_fit_resamples(conmat, behav, resamples, kfolds, covariates, thresh_method, thresh_level, bias_correct, network, return_edges, na_action)`
- Runner only orchestrates fold loops and result accumulation.

## Object Contracts

Core objects remain list-based S3:

1. `cpm_fit`: engine fit object used by parsnip prediction methods.
2. Internal resample results: a list with `folds`, `edges`, `metrics`,
   `predictions`, and `params`.

Required fields and semantics are documented in `R/core-objects.R` and
validated by tests.

## Testing Plan

1. Add contract tests for each core helper.
2. Keep anti-leakage invariants for covariate handling under resampling.
3. Cover both success and validation-error branches for input and resample
   helpers.
4. Maintain full test coverage for the core layer.

## Rollout Plan

1. Implement the module split in `R/core-*`.
2. Route the tidymodels engine through the core helpers.
3. Document the internal contracts in this RFC and in tests.
4. Keep future extensions additive to these contracts.

## Acceptance Criteria

1. Core CPM logic is isolated in `R/core-*` files.
2. The tidymodels adapter calls the core layer instead of duplicating logic.
3. Anti-leakage guarantees remain explicit and tested.
4. The core layer has full test coverage.
