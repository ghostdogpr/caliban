# Agent instructions

The first section is specific to this repository. The others are shared by all of ghostdogpr's Scala libraries and maintained in `ghostdogpr/.github`.

## This repository

caliban is a library for GraphQL servers and clients. It builds with sbt 1, and most modules are cross-built for Scala 2.12, 2.13 and 3. Scala 2.13 is the default version.

- Code in `src/main/scala` must compile with Scala 2.12, 2.13 and 3, so it cannot use syntax that only Scala 3 supports. Code for one version goes in `scala-2`, `scala-3`, `scala-2.12` or `scala-2.13`. Some modules support fewer versions, as their `crossScalaVersions` settings show.
- Select the Scala version with `++` in every command, such as `sbt --client "++3.9 rootJVM3/test"`. The sbt server keeps the last selected version, so a command without `++` runs with whatever version the previous command selected.
- Formatting depends on the Scala version, because the files under `scala-3` are only included under Scala 3. CI runs `sbt ++2.13 check ++3.9 check`, so format with `sbt ++2.13 fmt ++3.9 fmt`.
- Tests use zio-test. CI runs `rootJVM212/test`, `rootJVM213/test` and `rootJVM3/test` with the matching Scala version, and `clientJS/test` and `clientNative/test` for the client on Scala.js and Scala Native. The codegen sbt plugin has scripted tests: `++2.12 codegenSbt/scripted`, `++2.12 codegenScriptedScala3` and `++3.9 codegenSbt/scripted`.
- CI runs on CircleCI. `.circleci/config.yml` lists the exact commands, including the MiMa check in the `test_mima` job, which has no alias. The GitHub workflows run the Apollo Federation compatibility tests, the gateway audit and the documentation deployment.
- `++2.13 docs/mdoc` compiles the Scala snippets in the documentation, so they must compile.

## Consistency

Consistency matters more than local preference.

- Follow the conventions of the surrounding code: naming, layout, error and effect handling, API shape and test style. Represent effects the same way as the library's existing API.
- When a feature exists in several places, such as backends, platforms or Scala versions, change all of them the same way.
- Reuse the existing way of expressing a concept instead of adding a second one. If a convention looks wrong, ask before changing it everywhere.

## Design

Aim for less complexity with the same behavior. The best simplifications are ambitious: a data model in which invalid states cannot be written, two passes merged into one, a module that turns out to be unnecessary. Ask three questions of new code and of the code it touches:

- **Constraints** (Bjarnason): what freedom can be removed so that fewer incorrect implementations are possible? Use the least powerful type, signature or abstraction that works, and constrain values where they are created so that downstream checks disappear. Look for invalid states the types allow, invariants held by runtime checks or convention, types wider than their values, and unreachable defensive branches.
- **Simplicity** (Hickey): which independent concerns are entangled? Separate them so each can be understood alone. Look for one concept defined twice, caches kept apart from their data, fast paths woven into logic, stored fields that could be derived, and conditionals that encode a decision made elsewhere.
- **Parametricity** (Wadler): what does the type guarantee, and what depends on discipline? Prefer general, precise types, and avoid escape hatches such as `null`, casts, runtime type tests and exceptions. Look for unnamed tuples whose order matters, strings standing in for closed sets, and invariants stated only in comments.

A simplification is worth it when you can name what goes away: a check, a branch, a partial call, a parameter, a stored field, a duplicate definition or a convention to remember. Moving or renaming code, adding a branch for one more case, growing a refactoring, or adding work on a hot path does not count.

When you fix a bug, prefer a design that makes the bug impossible over a new branch that handles the failing case. If that redesign is too large, make the targeted fix and describe the redesign.

## Scala

- Do not call operations that throw on inputs the type allows, such as `.head`, `.get`, `Map#apply`, casts and non-exhaustive matches.
- Hot paths may use `null`, `var`, mutable collections and loops, kept inside one function or class behind a pure signature.
- Scala 3 code uses braces, because the builds compile with `-no-indent`.

## Building and testing

- Run sbt with `sbt --client <command>`. Run `sbt --client reload` when the build changes, and `sbt --client shutdown` before a command that needs new environment variables or JVM options.
- Run `fmt` before you finish. CI runs `check`.
- If MiMa reports a problem after an API change, describe it instead of adding a filter.

## Documentation and comments

- Write plain, direct prose for someone new to the library, in complete sentences. Avoid slogans, fragments, contrasts such as "A, not B", and decorative metaphors.
- Check the implementation before describing behavior. Keep details that help the reader and drop the rest.
- Comments explain why, not what. Keep comments added with a fix few and short, and leave existing documentation unchanged unless it becomes wrong.
- The documentation site is a VitePress site in `docs/`. Add new pages to the sidebar in `docs/.vitepress/config.mts`, and keep the `@VERSION@` placeholder in dependency snippets.
