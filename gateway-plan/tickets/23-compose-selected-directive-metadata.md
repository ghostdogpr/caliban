# 23 — Compose selected directive metadata

**Outcome:** `@tag` and custom directives selected with `@composeDirective` survive composition with their definitions, applications, and client coordinates intact.

**Blocked by:** 22 — Fail closed on security and unsupported Federation features

**Status:** ready-for-agent

## Completion criteria

- [ ] Federation `@tag` and `@composeDirective` are resolved through `@link` imports, aliases, and namespace-qualified names.
- [ ] Repeatable `@tag` applications are retained on every supported visible schema coordinate without changing ownership or routing.
- [ ] Each `@composeDirective(name: "@...")` declaration is validated, and the selected custom directive definition and applications are retained in the composed graph.
- [ ] Incompatible definitions, locations, repeatability, arguments, or applications across contributing subgraphs produce deterministic source-attributed diagnostics rather than first-source-wins behavior.
- [ ] Structural coordinate mappings and `@inaccessible` visibility are applied consistently to retained directive applications, so metadata cannot refer to a missing or source-only client coordinate.
- [ ] Client-schema introspection exposes composed directive definitions where standard GraphQL introspection permits it. Focused composition tests inspect applied metadata that standard introspection cannot expose.
- [ ] Composed directives remain metadata in this ticket; no generic runtime directive interpreter or serialized artifact is introduced.

