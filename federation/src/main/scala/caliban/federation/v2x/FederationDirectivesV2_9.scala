package caliban.federation.v2x

import caliban.InputValue
import caliban.InputValue.ListValue
import caliban.Value.{ BooleanValue, IntValue, StringValue }
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_9 extends FederationDirectivesV2_8 {

  def Cost(weight: Int) = Directive("cost", Map("weight" -> IntValue(weight)))

  case class GQLCost(weight: Int) extends GQLDirective(Cost(weight))

  def ListSize(
    assumedSize: Option[Int] = None,
    slicingArguments: Option[List[String]] = None,
    sizedFields: Option[List[String]] = None,
    requireOneSlicingArgument: Boolean = true
  ) = {
    val builder = Map.newBuilder[String, InputValue]

    assumedSize.foreach(size => builder += "assumedSize" -> IntValue(size))
    slicingArguments.foreach { args =>
      builder += "slicingArguments" -> ListValue(args.map(StringValue.apply))
    }
    sizedFields.foreach { fields =>
      builder += "sizedFields" -> ListValue(fields.map(StringValue.apply))
    }
    builder += "requireOneSlicingArgument" -> BooleanValue(requireOneSlicingArgument)

    Directive(
      "listSize",
      builder.result()
    )
  }

  case class GQLListSize(
    assumedSize: Option[Int] = None,
    slicingArguments: Option[List[String]] = None,
    sizedFields: Option[List[String]] = None,
    requireOneSlicingArgument: Boolean = true
  ) extends GQLDirective(
        ListSize(
          assumedSize,
          slicingArguments,
          sizedFields,
          requireOneSlicingArgument
        )
      )

}
