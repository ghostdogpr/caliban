package caliban

package object execution {
  private[caliban] def isIntrospectionField(field: Field): Boolean =
    field.name == "__schema" || field.name == "__type"

  private[caliban] def isMetaField(field: Field): Boolean =
    isIntrospectionField(field) || field.name == "__typename"
}
