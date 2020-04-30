# Validation
Caliban provides a little macro called `gqldoc` that can check at **compile-time** that a GraphQL query (a *document* to be exact) has valid syntax.

```scala
import caliban.Macros.gqldoc

val query = gqldoc("""
  query test {
    amos: character(name: "Amos Burton") {
      name
    }
  }""")
```

At **runtime**, it is possible to validate a query against your schema by calling the method `check` on your API.

```scala
def check(query: String): IO[CalibanError, Unit]
```

It is also possible to skip validation when executing a query by passing `skipValidation = true` when calling `execute` (also available in the adapters). This will slightly improve performance.