# Writing code

Write the smallest design that does the job. When the types can't represent a wrong state, the code can't reach one.

Solve edge cases by design rather than with extra branches. Pick a model where the edge case can't occur or is just an instance of the general case. When fixes keep adding special cases, the model is missing a concept.

## Design

Ask these three questions of new code and of the code it touches.

**Constraints** (Bjarnason, "Constraints Liberate, Liberties Constrain"). What freedom can we remove so fewer wrong implementations compile? Use the least powerful type that works. Constrain values where they're created, so the checks further down disappear.

**Simplicity** (Hickey, "Simple Made Easy"). Which concerns are tangled together? Pull them apart so you can understand each one without the other. Look for the same concept defined twice, caches kept apart from their data, and fields that could be computed.

**Parametricity** (Wadler, "Theorems for free!"). What does the type guarantee, and what relies on the developer being careful? Turn conventions and comment-only invariants into signatures, then delete the code that guarded them.

## Scala

**Idiomatic code.** Write functional Scala with immutable values, pure functions, ADTs and pattern matching, and effects tracked in the types. Make the type prove what the code relies on instead of calling operations that throw on inputs the type allows, such as `.head`, `.get`, `Map#apply`, casts or non-exhaustive matches.

**Hot paths.** Code on a hot path may use `null`, `var`, mutable collections and loops when they save work per call. Keep them inside one function or class behind a pure signature, so callers never see the mutation or the null.
