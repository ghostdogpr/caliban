# 19 — Add operation resolution and policy hooks

**Outcome:** Applications can resolve operation identifiers and reject validated operations through narrow, fail-closed hooks without replacing gateway execution.

**Blocked by:** 06 — Complete cross-source data and errors

**Status:** complete

## Completion criteria

- [x] Built-in request text remains the default operation resolver; an optional resolver may produce canonical query text from request metadata or an identifier.
- [x] An optional operation policy may allow or reject an already validated operation but cannot rewrite its schema, routes, or execution result.
- [x] Hook failures fail closed with safe gateway-authored errors.
- [x] Effectful hooks contribute their ZIO environment requirements to the gateway type and preserve trace context and interruption.
- [x] Stable resolver and policy discriminators participate in cache keys; unstable or non-default behavior can explicitly bypass caching.
- [x] Tests cover default behavior, resolved identifiers, rejection, hook failure, environment intersection, and cache bypass signaling.
- [x] No general middleware or authorization framework is introduced.
