# RFC 0001: CPM Core Engine Stabilization

- Status: Accepted
- Authors: cpmr maintainers
- Target release: 0.2.x
- Last updated: 2026-03-16

## Summary

This RFC defines the stable internal CPM engine used by the native `cpmr`
interfaces. The core layer is intentionally independent from any external
workflow framework and owns:

1. Input validation and normalization
2. Leakage-safe training/assessment preprocessing
3. Edge selection
4. Model training and prediction
5. Resample orchestration primitives
The purpose is to preserve correctness and performance while keeping any future
workflow adapter thin and optional.

## Motivation

The CPM hot path is dominated by matrix operations and fold-local orchestration.
To keep that path maintainable, leakage-safe, and performant, the package needs
explicit internal contracts that do not depend on generic workflow frameworks.

## Goals

1. Define clear, stable internal function contracts.
2. Keep anti-leakage behavior explicit and hard to accidentally break.
3. Support both native helper entry points and `cpm_spec()`-driven fitting
   through the same internal engine.
4. Improve maintainability by separating internal engine contracts from public
   API choices.

## Non-goals

1. Immediate removal of supported native user-facing APIs.
2. Adding new modeling families in this phase.
3. Forcing adapter dependencies in the core layer.

## Design Principles

1. Anti-leakage first: all parameter-learning steps remain inside training
   splits.
2. Deterministic contracts: every core helper documents expected shape/types.
3. Minimal coupling: core engine does not import external workflow packages.
4. Native-first: performance-sensitive resampling and future tuning stay under
   package-controlled orchestration.

## Proposed Internal Modules

The following files define the current internal organization:

1. `R/core-input.R`
2. `R/core-preprocess.R`
3. `R/core-select-edges.R`
4. `R/core-train-predict.R`
5. `R/core-resample-runner.R`
6. `R/core-objects.R`

An earlier phase temporarily used `core_*` helper names plus a
`compat-legacy-api.R` shim. Those transitional names have been removed; the
module boundaries remain.

## Internal Contracts

### 1) Input normalization and case selection

- `normalize_inputs(conmat, behav, covariates = NULL)`
- `resolve_include_cases(conmat, behav, covariates, na_action)`
- `na_action` in `c("fail", "exclude")`.
- These helpers normalize behavior/covariates and identify rows eligible for
  fitting or resampling.

### 2) Leakage-safe preprocessing

- `regress_covariates(resp, covariates)`
- `regress_covariates_by_train(resp_train, resp_test, cov_train, cov_test)`
- `prepare_training_data(conmat, behav, covariates, rows_train)`
- `prepare_assessment_data(conmat, behav, covariates, rows_train, rows_test, covariates_train = NULL)`
- If covariates are present, nuisance regression is fit on training rows only.

### 3) Edge selection

- `critical_r(n, alpha)`
- `select_edges(conmat_train, behav_train, thresh_method, thresh_level)`
- Returns logical matrix `n_edges x 2` with columns `pos` and `neg`.

### 4) Training and prediction

- `fscale(x, center, scale)`
- `train_model(conmat_train, behav_train, edges, bias_correct)`
- Returns trained components required for prediction.
- `predict_model(model, conmat_new)` returns matrix with columns:
  `both`, `pos`, `neg`.

### 5) Resample orchestration

- `validate_kfolds(kfolds)`
- `validate_resamples(resamples, include_cases)`
- `resolve_resample_folds(resamples, kfolds, include_cases)`
- `crossv_kfold(x, k)`
- `warn_large_edge_storage(n_edges, kfolds, return_edges)`
- `run_single_fit(spec, conmat, behav, covariates, return_edges, na_action, call = NULL)`
- `run_resample_fit(spec, conmat, behav, covariates, folds, return_edges, na_action)`
- Runner only orchestrates fold loops and result accumulation.

### 6) Object construction

- `new_cpm(call, behav, pred, edges, model, spec, params)`
- `new_cpm_resamples(spec, folds, edges, metrics, predictions, params)`
- Core objects remain list-based S3 objects during 0.2.x.

## Object Contracts

`cpm` and `cpm_resamples` remain the required object shapes for the public
native API. Required fields and semantics are documented in `R/core-objects.R`
and validated by tests.

## Testing Plan

1. Keep existing behavior snapshots for `cpm` and summary/tidy outputs.
2. Add contract tests for each core helper.
3. Add equivalence tests:
   public native API output == direct internal runner output.
4. Keep anti-leakage invariants for covariate handling under resampling.

## Rollout Plan

This RFC is implemented. Ongoing follow-up work should:

1. Keep public APIs thin over the core engine.
2. Preserve anti-leakage and performance invariants as tuning work evolves.
3. Avoid reintroducing redundant compatibility layers.
4. Document internal contract changes when module boundaries move.

## Acceptance Criteria

1. Existing public tests pass unchanged or with equivalent snapshots.
2. Core contract tests cover helper branches and fold-local leakage behavior.
3. `cpm_fit()`, `cpm_fit_resamples()`, `fit()`, and `fit_resamples()` all use
   the same internal engine.
4. No external workflow dependency is required for core operation.

