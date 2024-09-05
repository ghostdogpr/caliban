package poc.caliban.logrocket

import caliban._
import caliban.schema._
import caliban.schema.Schema
import caliban.wrappers.Wrappers._
import zio._
import caliban.schema.Annotations.GQLInterface

import scala.language.higherKinds

object GraphQLApi {
  type ZEff[A] = UIO[A]

  // https://blog.logrocket.com/handling-graphql-errors-like-a-champ-with-unions-and-interfaces/

  @GQLInterface
  sealed trait Error extends scala.Product with scala.Serializable {
    def message: String
  }

  final case class UserRegisterInput(login: String, email: String, password: String)
  final case class MutationUserRegisterArgs(input: UserRegisterInput)

  final case class User(id: String, login: String)

  sealed trait UserRegisterResult extends scala.Product with scala.Serializable

  final case class UserRegisterResultSuccess(user: User) extends UserRegisterResult
  final case class UserRegisterInvalidInputError(
    message: String,
    loginErrorMessage: scala.Option[String],
    emailErrorMessage: scala.Option[String],
    passwordErrorMessage: scala.Option[String]
  ) extends UserRegisterResult
      with Error
  final case class CountryBlockedError(message: String)  extends Error with UserRegisterResult

  final case class Query(
    _empty: ZEff[scala.Option[String]]
  )

  final case class Mutation(
    userRegister: MutationUserRegisterArgs => ZEff[UserRegisterResult]
  )

  val api: GraphQL[Any] = {
    import caliban.schema.Schema.auto._
    import caliban.schema.ArgBuilder.auto._

    graphQL[Any, Query, Mutation, Unit](
      RootResolver(
        Query(
          _empty = ZIO.succeed(None)
        ),
        Mutation(
          userRegister = args => LogrocketService.userRegister(args)
        )
      )
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors
  }

}
