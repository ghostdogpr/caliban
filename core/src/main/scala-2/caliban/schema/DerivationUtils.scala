package caliban.schema

import caliban.schema.Annotations.GQLValueType
import magnolia1.ReadOnlyCaseClass

private object DerivationUtils {

  def isValueType[F[_]](ctx: ReadOnlyCaseClass[F, ?]): Boolean =
    (ctx.isValueClass || ctx.annotations.exists(_.isInstanceOf[GQLValueType])) && ctx.parameters.nonEmpty

}
