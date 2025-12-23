package caliban.schema.fast

import caliban.schema.ArgBuilder

class FastArgBuilderWrapper[T](val value: ArgBuilder[T]) extends AnyVal
