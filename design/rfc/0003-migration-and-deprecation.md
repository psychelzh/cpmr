# RFC 0003: Migration and Deprecation Strategy

- Status: Superseded
- Authors: cpmr maintainers
- Depends on: RFC 0001, RFC 0002
- Target releases: historical
- Last updated: 2026-03-16

## Summary

This RFC described a compatibility-wrapper-first migration toward a
core-plus-tidymodels-adapter architecture. That is no longer the active
project direction.

It is retained only as historical context for the decisions made during the
early refactor.

## Objectives

The current direction is simpler:

1. Keep the native API surface front and center.
2. Use the internal core engine to serve both helper and `cpm_spec()` flows.
3. Avoid deprecation pressure unless two public paths are truly redundant.

## Historical Assumptions That No Longer Apply

1. There is no longer a planned standalone compatibility-wrapper layer.
2. There is no scheduled migration toward a tidymodels-first public API.
3. The package is not on a staged path to deprecate native workflows in favor
   of an adapter.

## Current Public Surface

Primary supported interfaces are:

1. `cpm_fit()` and `cpm_fit_resamples()` for the native matrix-first path.
2. `cpm_spec()` plus `fit()` / `fit_resamples()` for a lower-level native path.
3. `collect_metrics()`, `collect_predictions()`, `collect_edges()`,
   `summary()`, and `tidy()` for result inspection.

## Current Migration Guidance

1. Prefer documenting path selection over deprecation messaging.
2. Keep helper and spec-based native interfaces aligned on one engine.
3. Only discuss external adapters as optional interoperability.

## Deprecation Guidance Going Forward

1. Do not add deprecation warnings solely to push users toward a workflow
   adapter.
2. Consider deprecation only when a public API is both redundant and clearly
   replaced.
3. Keep performance-sensitive native paths stable unless there is a compelling
   reason to break them.

## Superseding Decision

This RFC is superseded by the native-first rollout decisions captured in the
current README, vignettes, implementation issues, and the accepted core-engine
RFC.

