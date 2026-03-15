# RFC 0003: Migration and Release Strategy

- Status: Implemented
- Authors: cpmr maintainers
- Depends on: RFC 0001, RFC 0002
- Target releases: 0.2.x -> 1.0.0
- Last updated: 2026-03-15

## Summary

This RFC defines the release posture for the tidymodels-first rewrite of
`cpmr`. The package now treats the development branch as an intentional API
reset rather than a long compatibility bridge for the previous interface.

## Objectives

1. Make the breaking rewrite explicit in docs and NEWS.
2. Keep the new API surface small and coherent around tidymodels.
3. Avoid silent leakage-sensitive behavior changes during the rewrite.

## Current User Surface

Primary functions after the rewrite:

1. `cpm_reg()`
2. `fit()`
3. `predict()`
4. `collect_edges()`
5. `cpm_cor()` and `cpm_spearman()`

## Target Architecture

1. Core engine: internal stable contracts (RFC 0001).
2. Tidymodels adapter: workflow integration (RFC 0002).
3. Documentation and release guidance that reflect the new API directly.

## Release Phases

### Phase A (0.2.x): Breaking rewrite on the development branch

1. Introduce `core-*` modules.
2. Introduce the tidymodels-facing API.
3. Remove stale legacy entry points and examples.

### Phase B (0.3.x): Adapter expansion

1. Expand workflow examples and tuning guides.
2. Add optional connectome-specific recipe steps if they remain justified.
3. Add grouped and nested resampling examples.

### Phase C (toward 1.0.0): Stabilization

1. Polish the parsnip engine and CPM-specific metrics.
2. Reassess which optional components should become stable public API.
3. Preserve only the abstractions that still reduce maintenance burden.

## Risk Register

1. Risk: users looking for removed legacy functions.
   Mitigation: make the breaking rewrite explicit in README, NEWS, and PR
   notes.
2. Risk: silent leakage regressions in adapter layer.
   Mitigation: cross-implementation equivalence tests and anti-leakage tests.
3. Risk: dependency burden for users who only need the core engine.
   Mitigation: keep workflow-related packages in `Suggests` initially.

## Documentation Plan

1. Add architecture RFC links in contributor docs.
2. Replace old examples with tidymodels-first examples.
3. Keep migration notes concise and focused on the new API.

## Release Checklist

Before each phase release:

1. Update `NEWS.md` with migration status.
2. Run full test suite and coverage checks.
3. Verify README and vignette examples against the new API.
4. Confirm `devtools::check()` is clean.

## Acceptance Criteria

1. The package presents a coherent tidymodels-first public API.
2. No stale legacy examples remain in package docs.
3. Leakage-safety guarantees remain explicit across release phases.
4. Validation and coverage are strong enough to support the rewrite.
