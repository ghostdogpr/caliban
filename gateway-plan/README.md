# Caliban Gateway Plan

This directory describes the next implementation of the embedded Caliban GraphQL gateway.

## Start here

An implementation agent reads, in order:

1. [IMPLEMENTATION-HANDOFF.md](IMPLEMENTATION-HANDOFF.md)
2. the assigned file in [tickets](tickets/)
3. [CONTEXT.md](CONTEXT.md) when domain terminology is unclear

The handoff owns product scope and implementation principles. The assigned ticket owns the current change. A ticket may refine private implementation details, but it may not introduce behavior assigned to a later ticket merely to make the current design look future-proof.

## Authority

- The handoff and current tickets are the implementation plan.
- [research](research/) records useful investigation of specifications and other gateways. It informs decisions but does not prescribe Caliban's internal architecture.
- [prototypes](prototypes/) records experiments. A prototype result is evidence to measure against the real gateway, not a requirement to reproduce its data structures.
- [SOURCES.md](SOURCES.md) records historical research inputs. Compatibility and benchmark integrations select and record current upstream revisions when their tickets begin.

## Direction

The gateway is an embedded, code-first Caliban library supporting ordinary remote GraphQL, Federation-enabled remote GraphQL, and in-process Caliban graphs. The implementation proceeds in vertical slices: each early ticket leaves a useful query working through `GatewayRuntime`.

The design is Caliban-first. Existing Caliban parsing, validation, execution, values, errors, and ZIO behavior are reused unless a concrete gateway requirement demonstrates that they are insufficient. Performance-specific representations are introduced behind existing seams after measurement.

## Roadmap

- Tickets 1–6 establish one complete remote and Federation path through structured Caliban responses.
- Tickets 7–9 add local graphs, ordinary lookups, and safe remote schema acquisition.
- Tickets 10–16 use the Federation Gateway Audit to drive routing, composition, and operation breadth.
- Tickets 17–21 add protocol and operation policy, bounded runtime behavior, and lifecycle closure.
- Tickets 22–24 close silent Federation semantics, preserve selected directive metadata, and secure the client exposure boundary.
- Tickets 25–26 integrate Quick and the encoded response path, Ticket 27 improves ambiguous-route planning, Ticket 28 measures and optimizes the production path, and Ticket 29 closes tracing, documentation, compatibility evidence, and release review.
