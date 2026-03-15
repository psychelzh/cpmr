# RFC 0002: Tidymodels Adapter for CPM

- Status: Implemented (Phase 1)
- Authors: cpmr maintainers
- Depends on: RFC 0001
- Target release: 0.2.x
- Last updated: 2026-03-15

## Summary

This RFC introduces the tidymodels-compatible adapter layer for `cpmr`. The
core idea is to treat CPM as a protocol implemented by a custom model
component rather than as a built-in standard regression model.

The implemented phase-1 adapter provides:

1. A parsnip model spec (`cpm_reg()`) with engine `"cpmr"`
2. Workflow compatibility for formulas and recipes
3. Yardstick-compatible CPM correlation metrics
4. Extraction helpers for CPM-specific artifacts (selected edges)
5. Threshold tuning through `tune_grid()`

## Motivation

`cpmr` users increasingly need standardized workflows for:

1. Group-aware CV and nested CV
2. Hyperparameter tuning
3. Metric aggregation and model comparison
4. Reproducible experiment orchestration

Tidymodels provides this outer framework, while CPM core logic must remain
inside fold-local training code to avoid leakage.

## Goals

1. Make CPM usable in workflows with `parsnip`, `workflows`, `tune`, and
   `yardstick`.
2. Keep CPM fold behavior aligned with the existing anti-leakage design.
3. Keep tidymodels dependencies optional where practical.

## Non-goals

1. Replacing core CPM logic with external algorithms.
2. Hiding CPM-specific assumptions under generic model abstractions.
3. Shipping every connectome-specific recipe step in the first adapter release.

## Proposed API

### Model specification

`cpm_reg(mode = "regression", engine = "cpmr", thresh_method = "alpha", thresh_level = 0.01, bias_correct = TRUE, network = "both")`

Notes:

1. `network`: `"both"`, `"pos"`, `"neg"` for prediction view.
2. `thresh_level`, `bias_correct`, and `network` can participate in tuning.
3. All tuning parameters must be applied in training splits only.

### Engine binding

1. Register model and engine with parsnip extension tools.
2. Implement engine fit method that calls RFC 0001 core functions.
3. Map the selected CPM network to parsnip `"numeric"` predictions.
4. Expose `"raw"` predictions with `both`, `pos`, and `neg` channels.

### Recipes steps

Phase 1 relies on standard recipes preprocessing. Connectome-specific custom
steps remain future work and should only be added when they can preserve
training-only parameter estimation.

### Metrics

1. `cpm_cor()`: Pearson correlation between truth and estimate.
2. `cpm_spearman()`: Spearman correlation variant.

Metrics should integrate with `yardstick::metric_set()`.

### Collectors

1. `collect_edges()`: extract selected edges from `cpm_fit`, `model_fit`, and
   `workflow` objects.
2. Multi-channel CPM predictions are available via `predict(..., type = "raw")`.

## Dependency Policy

1. Keep `recipes`, `rsample`, `tune`, `workflows`, and `dials` in `Suggests`
   initially.
2. Keep the core CPM engine independent from tidymodels packages.
3. Fail clearly when optional adapter packages are required but unavailable.

## Leakage-Safety Requirements

1. Feature selection must run within each analysis split only.
2. Covariate residualization parameters must be learned from training rows only.
3. Any scaling or threshold-learning parameters must be training-local.
4. Nested CV is required for unbiased model selection when tuning many options.

## Testing Plan

1. Unit tests for model registration and translation.
2. Integration tests for parsnip fit -> predict -> metric paths.
3. Workflow tests for recipe preprocessing and edge extraction.
4. Tuning tests with `tune_grid()` and `metric_set()`.

## Rollout Plan

1. Ship minimal adapter with `cpm_reg()` and one stable engine.
2. Add custom metrics and edge collectors.
3. Document formula, recipe, and tuning workflows.
4. Add connectome-specific recipe steps later if still needed.

## Acceptance Criteria

1. Users can fit CPM through `parsnip::fit()`.
2. Users can place CPM inside `workflows` and `tune_grid()`.
3. CPM metrics work inside `yardstick::metric_set()`.
4. Fold-local behavior remains anti-leakage consistent with RFC 0001.
