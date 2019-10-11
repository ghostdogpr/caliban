# Validation
It is possible to validate a query without running by calling the method `check` on your interpreter.

It is also possible to skip validation when executing a query by passing `skipValidation = true` when calling `execute`. This will slightly improve performance.