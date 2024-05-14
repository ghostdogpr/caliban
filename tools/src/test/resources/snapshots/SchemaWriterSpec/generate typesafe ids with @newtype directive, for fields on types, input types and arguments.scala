import Types._

import caliban.schema.Annotations._

import caliban.Value._
import caliban.parsing.adt.Directive
import caliban.schema.{ ArgBuilder, Schema }

object Types {
  final case class QueryGetFooArgs(
    id: CustomId,
    maybeId: scala.Option[ACustomIdOpt],
    mapbeAllIDsOpt: scala.Option[List[scala.Option[AMaybeInnerIdOpt]]]
  )
  final case class MutationUpdateFooArgs(foo: FooInput)
  case class ACustomIdOpt(value: String) extends AnyVal
  object ACustomIdOpt     {
    implicit val schema: Schema[Any, ACustomIdOpt]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[ACustomIdOpt] = implicitly[ArgBuilder[String]].map(ACustomIdOpt(_))
  }
  case class AMaybeInnerIdOpt(value: String) extends AnyVal
  object AMaybeInnerIdOpt {
    implicit val schema: Schema[Any, AMaybeInnerIdOpt]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[AMaybeInnerIdOpt] = implicitly[ArgBuilder[String]].map(AMaybeInnerIdOpt(_))
  }
  case class CustomFId(value: FID) extends AnyVal
  object CustomFId        {
    implicit val schema: Schema[Any, CustomFId]    = implicitly[Schema[Any, FID]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[CustomFId] = implicitly[ArgBuilder[FID]].map(CustomFId(_))
  }
  case class CustomId(value: String) extends AnyVal
  object CustomId         {
    implicit val schema: Schema[Any, CustomId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[CustomId] = implicitly[ArgBuilder[String]].map(CustomId(_))
  }
  case class CustomIdOpt(value: String) extends AnyVal
  object CustomIdOpt      {
    implicit val schema: Schema[Any, CustomIdOpt]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[CustomIdOpt] = implicitly[ArgBuilder[String]].map(CustomIdOpt(_))
  }
  case class CustomIntId(value: Int) extends AnyVal
  object CustomIntId      {
    implicit val schema: Schema[Any, CustomIntId]    = implicitly[Schema[Any, Int]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[CustomIntId] = implicitly[ArgBuilder[Int]].map(CustomIntId(_))
  }
  case class CustomStrId(value: String) extends AnyVal
  object CustomStrId      {
    implicit val schema: Schema[Any, CustomStrId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[CustomStrId] = implicitly[ArgBuilder[String]].map(CustomStrId(_))
  }
  case class IInnerId(value: String) extends AnyVal
  object IInnerId         {
    implicit val schema: Schema[Any, IInnerId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[IInnerId] = implicitly[ArgBuilder[String]].map(IInnerId(_))
  }
  case class IMaybeInnerIdOpt(value: String) extends AnyVal
  object IMaybeInnerIdOpt {
    implicit val schema: Schema[Any, IMaybeInnerIdOpt]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[IMaybeInnerIdOpt] = implicitly[ArgBuilder[String]].map(IMaybeInnerIdOpt(_))
  }
  case class InnerId(value: String) extends AnyVal
  object InnerId          {
    implicit val schema: Schema[Any, InnerId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[InnerId] = implicitly[ArgBuilder[String]].map(InnerId(_))
  }
  case class InnerOptId(value: String) extends AnyVal
  object InnerOptId       {
    implicit val schema: Schema[Any, InnerOptId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[InnerOptId] = implicitly[ArgBuilder[String]].map(InnerOptId(_))
  }
  case class MaybeInnerId(value: String) extends AnyVal
  object MaybeInnerId     {
    implicit val schema: Schema[Any, MaybeInnerId]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[MaybeInnerId] = implicitly[ArgBuilder[String]].map(MaybeInnerId(_))
  }
  case class MaybeInnerIdOpt(value: String) extends AnyVal
  object MaybeInnerIdOpt  {
    implicit val schema: Schema[Any, MaybeInnerIdOpt]    = implicitly[Schema[Any, String]].contramap(_.value)
    implicit val argBuilder: ArgBuilder[MaybeInnerIdOpt] = implicitly[ArgBuilder[String]].map(MaybeInnerIdOpt(_))
  }
  final case class Foo(
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomId"))))
    id: CustomId,
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomStrId"))))
    strId: CustomStrId,
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomIntId"))))
    intId: CustomIntId,
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomFId"))))
    fid: CustomFId,
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomIdOpt"))))
    maybeId: scala.Option[CustomIdOpt],
    IDs: List[String],
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("InnerId"))))
    allIDs: List[InnerId],
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("InnerOptId"))))
    allIDsOpt: List[scala.Option[InnerOptId]],
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("MaybeInnerId"))))
    mapbeAllIDs: scala.Option[List[MaybeInnerId]],
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("MaybeInnerIdOpt"))))
    mapbeAllIDsOpt: scala.Option[List[scala.Option[MaybeInnerIdOpt]]]
  )
  final case class FooInput(
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("CustomId"))))
    id: CustomId,
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("IInnerId"))))
    allIDs: List[IInnerId],
    @GQLDirective(Directive("newtype", Map("name" -> StringValue("IMaybeInnerIdOpt"))))
    mapbeAllIDsOpt: scala.Option[List[scala.Option[IMaybeInnerIdOpt]]]
  )

}

object Operations {

  final case class Query(
    getFoo: QueryGetFooArgs => zio.UIO[scala.Option[Foo]]
  )

  final case class Mutation(
    updateFoo: MutationUpdateFooArgs => zio.UIO[scala.Option[Foo]]
  )

}
