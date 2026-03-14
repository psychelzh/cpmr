# RFC 0003: Migration and Deprecation Strategy

- Status: Proposed
- Authors: cpmr maintainers
- Depends on: RFC 0001, RFC 0002
- Target releases: 0.2.x -> 1.0.0
- Last updated: 2026-03-14

## Summary

This RFC defines a phased migration path from the current `cpmr` API to a
core-plus-adapter architecture. It minimizes disruption by introducing
compatibility wrappers first and delaying hard deprecations until clear
migration paths exist.

## Objectives

1. Preserve existing user workflows while new architecture lands.
2. Provide explicit migration timeline and stable deprecation messaging.
3. Avoid silent behavior changes in leakage-sensitive code paths.

## Current User Surface

Primary functions in use today:

1. `cpm_spec()`
2. `fit(cpm_spec(), ...)`
3. `fit_resamples(cpm_spec(), ...)`
4. `collect_metrics()`, `collect_predictions()`, `collect_edges()`
5. `summary()`, `tidy()`

## Target Architecture

1. Core engine: internal stable contracts (RFC 0001).
2. Tidymodels adapter: workflow integration (RFC 0002).
3. Legacy compatibility layer: existing API forwarding to core engine.

## Migration Phases

### Phase A (0.2.x): Internal stabilization, no user breakage

1. Introduce `core-*` modules.
2. Route existing exported API through compatibility wrappers.
3. Keep user arguments and result structures unchanged.

### Phase B (0.3.x): Adapter introduction

1. Add tidymodels-facing APIs (for example `cpm_reg()` and related helpers).
2. Keep legacy APIs fully supported.
3. Add migration guides that map old usage to new workflows.

### Phase C (0.4.x to 0.9.x): Soft deprecation window

1. Mark legacy-only entry points as superseded where appropriate.
2. Emit lifecycle-style deprecation warnings only when alternatives are mature.
3. Maintain compatibility for at least two minor releases after warning starts.

### Phase D (1.0.0): Decide long-term support posture

1. Keep compatibility wrappers if maintenance cost is low.
2. Otherwise, perform documented removals that were pre-announced in 0.x.
3. Preserve conversion guidance and examples in docs.

## Deprecation Policy

1. No immediate hard removals for currently documented workflows.
2. Every deprecation warning must include:
   replacement API + one-line migration hint.
3. Major behavioral changes require explicit NEWS and vignette updates.

## Risk Register

1. Risk: naming conflict with tidymodels verbs (`fit_resamples`,
   `collect_metrics`).
   Mitigation: document namespace strategy and prefer explicit package calls in
   examples.
2. Risk: silent leakage regressions in adapter layer.
   Mitigation: cross-implementation equivalence tests and anti-leakage tests.
3. Risk: dependency burden for current users.
   Mitigation: keep adapter dependencies optional (`Suggests`) initially.

## Documentation Plan

1. Add architecture RFC links in contributor docs.
2. Add side-by-side migration examples in vignettes.
3. Include a "Which API should I use?" decision table in README/vignette.

## Release Checklist

Before each phase release:

1. Update `NEWS.md` with migration status.
2. Run full test suite and snapshot checks.
3. Verify examples in both legacy and adapter docs.
4. Confirm deprecation warnings are actionable and non-ambiguous.

## Acceptance Criteria

1. Existing users can stay on current API with no immediate rewrite.
2. New users can adopt tidymodels workflow once adapter is released.
3. Deprecation timeline is clear, documented, and honored.
4. Leakage-safety guarantees remain explicit across all phases.

