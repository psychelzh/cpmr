# RFC 0002: Tidymodels Adapter for CPM

- Status: Deferred
- Authors: cpmr maintainers
- Depends on: RFC 0001
- Target release: none scheduled
- Last updated: 2026-03-16

## Summary

This RFC records a possible tidymodels-compatible interoperability layer for
`cpmr`, but that work is no longer on the primary package roadmap.

If revisited, the adapter must remain optional, thin, and clearly secondary to
the native `cpmr` workflow.

## Motivation

Some users may still want standardized workflows for:

1. Group-aware CV and nested CV
2. Hyperparameter tuning
3. Metric aggregation and model comparison
4. Reproducible experiment orchestration

However, the project now treats high-dimensional resampling and CPM-specific
tuning as native-package responsibilities because generic workflow
orchestration adds meaningful overhead in the hot path.

## Goals

1. Offer optional interoperability for users who specifically need tidymodels
   tooling.
2. Keep CPM fold behavior aligned with the native anti-leakage design.
3. Avoid forcing adapter dependencies on users who only need core `cpmr`.

## Non-goals

1. Making tidymodels the primary package identity.
2. Recommending generic workflow orchestration for performance-critical
   resampling or tuning-heavy CPM analyses.
3. Expanding adapter scope before the native path is stable and well
   documented.

## Design Constraints If Revisited

1. Any adapter must call the native core engine rather than duplicate CPM
   orchestration logic.
2. Adapter dependencies must remain optional (`Suggests`).
3. Documentation must explain that adapter convenience does not imply parity
   with the native path for heavy resampling or threshold tuning.
4. Fold-local leakage guarantees must remain explicit and testable.

## Dependency Policy

1. Keep tidymodels-related packages in `Suggests` initially.
2. Fail gracefully with clear errors when adapter functions are called without
   required packages.
3. Keep native `cpmr` workflows fully functional without tidymodels installed.

## Leakage-Safety Requirements

1. Feature selection must run within each analysis split only.
2. Covariate residualization parameters must be learned from training rows only.
3. Any scaling or threshold-learning parameters must be training-local.
4. If tuning support exists, docs must state when nested CV is required for
   unbiased model selection.

## Testing Plan

1. Unit tests for model registration and translation.
2. Integration tests for fit -> predict -> metric paths.
3. Equivalence tests versus direct native runners under matched split
   definitions.
4. Explicit regression tests for any adapter-specific leakage hazards.

## Rollout Plan

No rollout is scheduled. This RFC should only move back to `Proposed` if:

1. the native-first roadmap is stable;
2. there is clear user demand for interoperability;
3. adapter maintenance cost is justified by that demand.

## Acceptance Criteria

1. The adapter is demonstrably optional rather than central to package use.
2. Fold-local behavior remains anti-leakage consistent with RFC 0001.
3. Adapter docs set correct expectations about performance and scope.
4. The native path remains the documented recommendation for heavy resampling.

