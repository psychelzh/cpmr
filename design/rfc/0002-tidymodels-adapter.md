# RFC 0002: Tidymodels Adapter for CPM

- Status: Proposed
- Authors: cpmr maintainers
- Depends on: RFC 0001
- Target release: 0.3.x
- Last updated: 2026-03-14

## Summary

This RFC introduces a tidymodels-compatible adapter layer for `cpmr`. The core
idea is to treat CPM as a protocol implemented by a custom model component
rather than as a built-in standard regression model.

The adapter layer will provide:

1. A parsnip model spec (`cpm_reg()`) with engine `"cpmr"`
2. Optional recipes steps for connectome-specific preprocessing
3. Yardstick-compatible CPM correlation metrics
4. Extraction helpers for CPM-specific artifacts (for example selected edges)

## Motivation

`cpmr` users increasingly need standardized workflows for:

1. Group-aware CV and nested CV
2. Hyperparameter tuning
3. Metric aggregation and model comparison
4. Reproducible experiment orchestration

Tidymodels provides this outer framework, while CPM core logic must remain
inside fold-local training code to avoid leakage.

## Goals

1. Make CPM usable in workflows with `rsample`, `tune`, and `yardstick`.
2. Keep CPM fold behavior aligned with the existing anti-leakage design.
3. Avoid forcing tidymodels dependencies for users who only need core `cpmr`.

## Non-goals

1. Replacing core modeling logic with external algorithms.
2. Hiding CPM-specific assumptions under generic model abstractions.
3. Shipping every possible preprocessing step in the first adapter release.

## Proposed API

### Model specification

`cpm_reg(mode = "regression", thresh_method = tune(), thresh_level = tune(), bias_correct = tune(), network = tune(), method = tune())`

Notes:

1. `network`: `"both"`, `"pos"`, `"neg"` for prediction view.
2. `method`: initially `"lm"` (with optional future `"pls"` extension).
3. All tuning parameters must be applied in training splits only.

### Engine binding

1. Register model and engine with parsnip extension tools.
2. Implement engine fit method that calls RFC 0001 core functions.
3. Ensure prediction columns map cleanly to parsnip outputs.

### Recipes steps (initial set)

1. `step_upper_tri()`: convert symmetric connectivity input to edge vector.
2. `step_fisher_z()`: Fisher-z transform for correlation-like features.
3. `step_residualize()`: optional nuisance-residual transform (training-only
   coefficient estimation).

These steps should be introduced incrementally. If a step is not fully
leakage-safe in generic recipe execution, keep that transform in the model
engine path instead.

### Metrics

1. `metric_cpm_cor()`: Pearson correlation between truth and estimate.
2. `metric_cpm_spearman()`: Spearman correlation variant.

Metrics should integrate with `yardstick::metric_set()`.

### Collectors

1. `collect_cpm_edges()`: extract selected edges from fitted workflow/resample
   objects.
2. `collect_cpm_predictions()`: harmonized accessor when CPM returns multiple
   prediction channels.

## Dependency Policy

1. Keep tidymodels-related packages in `Suggests` initially.
2. Fail gracefully with clear errors when adapter functions are called without
   required packages.
3. Keep core `fit()/fit_resamples()` functional without tidymodels installed.

## Leakage-Safety Requirements

1. Feature selection must run within each analysis split only.
2. Covariate residualization parameters must be learned from training rows only.
3. Any scaling or threshold-learning parameters must be training-local.
4. Nested CV is required for unbiased model selection when tuning many options.

## Testing Plan

1. Unit tests for model registration and translation.
2. Integration tests:
   parsnip fit -> predict -> metric path.
3. Resampling tests with `vfold_cv`, `group_vfold_cv`, and nested structure.
4. Equivalence tests versus direct core runner under matched split definitions.

## Rollout Plan

1. Ship minimal adapter with `cpm_reg()` and one stable engine.
2. Add custom metrics and basic collectors.
3. Add optional recipe steps incrementally with explicit leakage tests.
4. Promote adapter docs in vignette after reliability baseline is met.

## Acceptance Criteria

1. Users can tune core CPM thresholds inside tidymodels workflows.
2. Fold-local behavior remains anti-leakage consistent with RFC 0001.
3. Adapter does not break existing non-tidymodels API.
4. Documentation includes grouped and nested CV examples.

