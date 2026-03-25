---
name: r-api-design
description: Design or refactor R function APIs with tidyverse-inspired principles. Use when choosing function names, argument order, defaults, enums vs booleans, options/helper objects, output contracts, side effects, or error structure in R packages.
---

# R API Design

Use this skill when shaping an R API, not just when polishing style.
Optimize for humans reading code, consistency across related functions, and
small composable pieces.

Before editing, read the public function signature, its closest tests, and the
callers directly above and below it in the stack.

## Design Priorities

1. Keep layers distinct.
   Do not force one naming pattern across different abstraction levels. Public
   entry points, orchestration helpers, and low-level transforms can use
   different name families if the boundary becomes clearer.
2. Prefer interfaces that are obvious at a glance.
   The function signature should reveal what is required, what is optional, and
   which arguments matter most.
3. Optimize for consistency and composition.
   Reuse the same API pattern across related functions, and make outputs easy to
   feed into the next step.
4. Prefer explicit behavior.
   Avoid hidden dependence on options, locale, or internal state when it affects
   computation.

## Naming

- Use `snake_case`.
- Prefer verbs for actions and nouns for object builders.
- Err toward names that are a bit long but obvious.
- Reserve very short names for very frequent calls.
- Use prefixes for families and suffixes for variations.
- Keep related names parallel within a layer, but do not flatten different
  layers into one naming scheme just for symmetry.

## Function Signatures

- Put the most important arguments first.
- Give defaults only to optional arguments.
- Put `...` after required arguments.
- Keep defaults short and simple.
  If the real default is non-trivial, prefer `NULL` plus in-body resolution, or
  a documented helper object.
- Make inputs explicit.
  If an option, locale, or external setting changes results, surface it as an
  argument.
- Keep argument meanings independent whenever possible.
  Avoid signatures where one argument silently changes the meaning of another.

## Options And Strategies

- Use small string enums for a finite set of choices.
  Prefer `c("a", "b")` with `rlang::arg_match()` or `match.arg()` over ad hoc
  checking.
- Prefer enums over booleans when `TRUE`/`FALSE` would be ambiguous, or when
  the choice may grow beyond two strategies.
- If different strategies need different arguments, extract them into helper
  objects instead of piling mutually dependent arguments into one function.
- If low-importance tuning arguments clutter a signature, group them in an
  options object created by a helper.

## Helpers

- Delete helpers that only forward arguments or rename values without adding a
  clear boundary.
- Keep a private helper when it collapses truly identical repetition or enforces
  one stable invariant.
- Extract an error helper only after the same error pattern appears in three or
  more places.
- Place private helpers after the main function(s) they support so the file
  reads top-down from public flow to implementation detail.

## Outputs, Side Effects, And Errors

- Keep outputs type-stable whenever possible.
- If a function returns multiple values, give them stable names and a stable
  container shape.
- Partition side effects away from computation where possible.
- If a function exists mainly for side effects, return invisibly.
- Suppress noisy raw calls in user-facing errors with `call. = FALSE` or
  `rlang::abort()`.
- Use custom error constructors only for repeated, structured failure modes.

## Review Loop

When reviewing or refactoring an R API:

1. Identify the abstraction layer of the function before judging its name.
2. Check whether the signature is scannable without reading documentation.
3. Replace boolean switches or tangled arguments with clearer strategy choices
   only when that actually reduces complexity.
4. Remove helpers that exist only because of prior refactors, not because they
   carry meaning now.
5. Re-check callers and tests after renaming so the new boundary is still
   obvious in use.

For deeper source-backed notes, read
[references/tidyverse-design-map.md](references/tidyverse-design-map.md).
