package caliban

import caliban.schema.Annotations.GQLDescription

object RenderingSpecSchemaSingleLineDescription {

  @GQLDescription("type description in a single line")
  case class OutputValue(@GQLDescription("field description in a single line") r: Int)

  case class InputValue(@GQLDescription("argument description in a single line") in: Int)

  def getResult: OutputValue = ???

  case class Query(
    @GQLDescription("query description in a single line") q: InputValue => OutputValue
  )

  val resolver = RootResolver(
    Query(_ => getResult)
  )
}

object RenderingSpecSchemaMultiLineDescription {

  @GQLDescription("type description in\nMultiple lines")
  case class OutputValue(@GQLDescription("field description in\nMultiple lines") r: Int)

  case class InputValue(@GQLDescription("argument description in\nMultiple lines") in: Int)

  def getResult: OutputValue = ???

  case class Query(
    @GQLDescription("query description in\nMultiple lines") q: InputValue => OutputValue
  )

  val resolver = RootResolver(
    Query(_ => getResult)
  )
}

object RenderingSpecSchemaSingleLineEndingInQuoteDescription {

  @GQLDescription("type description in a single line \"ending in quote\"")
  case class OutputValue(@GQLDescription("field description in a single line \"ending in quote\"") r: Int)

  case class InputValue(@GQLDescription("argument description in a single line \"ending in quote\"") in: Int)

  def getResult: OutputValue = ???

  case class Query(
    @GQLDescription("query description in a single line \"ending in quote\"") q: InputValue => OutputValue
  )

  val resolver = RootResolver(
    Query(_ => getResult)
  )
}

object RenderingSpecSchemaMultiLineEndingInQuoteDescription {

  @GQLDescription("type description in multiple lines\n\"ending in quote\"")
  case class OutputValue(@GQLDescription("field description in multiple lines\n\"ending in quote\"") r: Int)

  case class InputValue(@GQLDescription("argument description in multiple lines\n\"ending in quote\"") in: Int)

  def getResult: OutputValue = ???

  case class Query(
    @GQLDescription("query description in multiple lines\n\"ending in quote\"") q: InputValue => OutputValue
  )

  val resolver = RootResolver(
    Query(_ => getResult)
  )
}
