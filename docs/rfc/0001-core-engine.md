# RFC 0001: CPM Core Engine Stabilization

- Status: Proposed
- Authors: cpmr maintainers
- Target release: 0.2.x
- Last updated: 2026-03-14

## Summary

This RFC proposes a stable internal "core engine" layer for `cpmr` that is
independent from any external workflow framework. The core layer will own:

1. Input validation and normalization
2. Leakage-safe training/assessment preprocessing
3. Edge selection
4. Model training and prediction
5. Resample orchestration primitives

The purpose is to preserve correctness and performance while making later
tidymodels integration a thin adapter instead of a rewrite.

## Motivation

The current code already separates many CPM primitives, but contracts are still
implicit and distributed across files. To support larger extensions (custom
resampling objects, tidymodels adapters, richer metrics) without behavior
drift, we need explicit and testable internal interfaces.

## Goals

1. Define clear, stable internal function contracts.
2. Keep anti-leakage behavior explicit and hard to accidentally break.
3. Preserve user-facing behavior for `fit(cpm_spec(), ...)` and
   `fit_resamples(cpm_spec(), ...)` during 0.2.x.
4. Improve maintainability by separating "core engine" and "API compatibility"
   responsibilities.

## Non-goals

1. Immediate removal of existing user-facing API.
2. Adding new modeling families in this phase.
3. Forcing tidymodels dependencies in the core layer.

## Design Principles

1. Anti-leakage first: all parameter-learning steps remain inside training
   splits.
2. Deterministic contracts: every core helper documents expected shape/types.
3. Minimal coupling: core engine does not import tidymodels packages.
4. Backward compatibility: legacy API becomes a thin wrapper over core engine.

## Proposed Internal Modules

The following files define the target internal organization:

1. `R/core-input.R`
2. `R/core-preprocess.R`
3. `R/core-select-edges.R`
4. `R/core-train-predict.R`
5. `R/core-resample-runner.R`
6. `R/core-objects.R`
7. `R/compat-legacy-api.R`

Existing files may temporarily re-export or delegate to these modules during
migration.

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

- `core_select_edges(conmat_train, behav_train, thresh_method, thresh_level)`
- Returns logical matrix `n_edges x 2` with columns `pos` and `neg`.

### 5) Training and prediction

- `core_train_model(conmat_train, behav_train, edges, bias_correct)`
- Returns trained components required for prediction.
- `core_predict_model(model, conmat_new)` returns matrix with columns:
  `both`, `pos`, `neg`.

### 6) Resample runner

- `core_fit_single(spec, conmat, behav, covariates, return_edges, na_action)`
- `core_fit_resamples(spec, conmat, behav, covariates, folds, return_edges, na_action)`
- Runner only orchestrates fold loops and result accumulation.

## Object Contracts

Core objects remain list-based S3 during 0.2.x:

1. `cpm`: single-fit results.
2. `cpm_resamples`: resampling results.

Required fields and semantics are documented in `R/core-objects.R` and
validated by tests.

## Testing Plan

1. Keep existing behavior snapshots for `cpm` and summary/tidy outputs.
2. Add contract tests for each core helper.
3. Add equivalence tests:
   legacy wrapper output == direct core runner output.
4. Keep anti-leakage invariants for covariate handling under resampling.

## Rollout Plan

1. Move logic into `core-*` modules with no behavior changes.
2. Redirect existing exported methods to new core functions.
3. Keep current function names and arguments stable.
4. Ship with migration notes in NEWS and vignettes.

## Acceptance Criteria

1. Existing public tests pass unchanged or with equivalent snapshots.
2. New core contract tests cover all helper branches.
3. `fit()` and `fit_resamples()` output structures remain backward compatible.
4. No new required package dependencies for core operation.

