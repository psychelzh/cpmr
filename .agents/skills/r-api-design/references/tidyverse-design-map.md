# Tidyverse Design Source Map

This note distills the parts of [Tidy design principles](https://design.tidyverse.org/)
that are most useful when designing or refactoring R package APIs.
Use it as a map back to the official source, not as a substitute for the book.

## Core frame

- [Unifying principles](https://design.tidyverse.org/unifying.html)
  Optimize for humans, consistency, composition, and inclusive interfaces.
- [Function names](https://design.tidyverse.org/function-names.html)
  Prefer clear `snake_case` names, usually verbs, with prefixes for families and
  suffixes for variations.

## Signatures

- [Put the most important arguments first](https://design.tidyverse.org/important-args-first.html)
  Order arguments by importance and typical use, not by implementation details.
- [Required args shouldn’t have defaults](https://design.tidyverse.org/required-no-defaults.html)
  Make requiredness visible from the signature.
- [Put ... after required arguments](https://design.tidyverse.org/dots-after-required.html)
  Keep the structure of the call readable.
- [Keep defaults short and sweet](https://design.tidyverse.org/defaults-short-and-sweet.html)
  Prefer simple defaults; move complex resolution into the body or a helper.
- [Avoid magical defaults](https://design.tidyverse.org/def-magical.html)
  Do not make an explicitly supplied default behave differently from an omitted
  argument.
- [Make inputs explicit](https://design.tidyverse.org/inputs-explicit.html)
  Avoid hidden dependence on global options, locale, or other ambient state.

## Choices and helper objects

- [Enumerate possible options](https://design.tidyverse.org/enumerate-options.html)
  Use small string enums for finite choices and validate them centrally.
- [Prefer a enum, even if only two choices](https://design.tidyverse.org/boolean-strategies.html)
  Avoid booleans when `TRUE`/`FALSE` would not be self-explanatory or the design
  may grow.
- [Reduce argument clutter with an options object](https://design.tidyverse.org/argument-clutter.html)
  Move low-importance tuning knobs into a helper-built object when the main
  signature gets noisy.
- [Extract strategies into objects](https://design.tidyverse.org/strategy-objects.html)
  When different strategies need different arguments, use helper objects rather
  than one oversized signature.

## Outputs and side effects

- [Type-stability](https://design.tidyverse.org/out-type-stability.html)
  Make output type predictable from input types, not surprising value-dependent
  branches.
- [Side-effect functions should return invisibly](https://design.tidyverse.org/out-invisible.html)
  Let side-effect helpers stay pipe-friendly without pretending they compute a
  primary value.
- [Side-effect soup](https://design.tidyverse.org/side-effects.html)
  Partition side effects deliberately; mixing them with transformation logic
  raises cognitive load fast.

## Errors

- [Error call](https://design.tidyverse.org/err-call.html)
  Hide noisy raw calls from user-facing errors unless the call is genuinely
  helpful.
- [Error constructors](https://design.tidyverse.org/err-constructor.html)
  Extract structured error helpers only when the same failure pattern repeats
  enough to justify the machinery.
