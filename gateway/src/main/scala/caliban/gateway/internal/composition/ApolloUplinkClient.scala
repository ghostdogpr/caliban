package caliban.gateway.internal.composition

import caliban.client.FieldBuilder.{ ChoiceOf, Obj, Scalar }
import caliban.client.Operations.RootQuery
import caliban.client.{ Argument, SelectionBuilder }

object ApolloUplinkClient {

  sealed trait RouterConfig

  object RouterConfig {
    case class Success(id: String, supergraphSDL: Option[String], minDelaySeconds: Double) extends RouterConfig
    case class Failed(code: String, message: String)                                       extends RouterConfig
  }

  def supergraphSDL(
    apiKey: String,
    ref: String,
    ifAfterId: Option[String]
  ): SelectionBuilder[RootQuery, RouterConfig] =
    Query.routerConfig(apiKey, ref, ifAfterId)(
      onRouterConfigResult =
        (RouterConfigResult.id ~ RouterConfigResult.supergraphSDL.map(Some(_)) ~ RouterConfigResult.minDelaySeconds)
          .mapN(RouterConfig.Success.apply _),
      onFetchError = (FetchError.code ~ FetchError.message).mapN(RouterConfig.Failed.apply _),
      onUnchanged = (Unchanged.id ~ SelectionBuilder.pure(None) ~ Unchanged.minDelaySeconds)
        .mapN(RouterConfig.Success.apply _)
    )

  private[ApolloUplinkClient] type RouterConfigResult
  private object RouterConfigResult {
    def id: SelectionBuilder[RouterConfigResult, String]              = SelectionBuilder.Field("id", Scalar())
    def supergraphSDL: SelectionBuilder[RouterConfigResult, String]   = SelectionBuilder.Field("supergraphSDL", Scalar())
    def minDelaySeconds: SelectionBuilder[RouterConfigResult, Double] =
      SelectionBuilder.Field("minDelaySeconds", Scalar())
  }

  private[ApolloUplinkClient] type FetchError
  private object FetchError {
    def code: SelectionBuilder[FetchError, String]    = SelectionBuilder.Field("code", Scalar())
    def message: SelectionBuilder[FetchError, String] = SelectionBuilder.Field("message", Scalar())
  }

  private[ApolloUplinkClient] type Unchanged
  private object Unchanged {
    def id: SelectionBuilder[Unchanged, String]              = SelectionBuilder.Field("id", Scalar())
    def minDelaySeconds: SelectionBuilder[Unchanged, Double] = SelectionBuilder.Field("minDelaySeconds", Scalar())
  }

  private type Query = RootQuery
  private object Query {
    def routerConfig[A](apiKey: String, ref: String, ifAfterId: Option[String])(
      onRouterConfigResult: SelectionBuilder[RouterConfigResult, A],
      onFetchError: SelectionBuilder[FetchError, A],
      onUnchanged: SelectionBuilder[Unchanged, A]
    ): SelectionBuilder[RootQuery, A] =
      SelectionBuilder.Field(
        "routerConfig",
        ChoiceOf(
          Map(
            "RouterConfigResult" -> Obj(onRouterConfigResult),
            "FetchError"         -> Obj(onFetchError),
            "Unchanged"          -> Obj(onUnchanged)
          )
        ),
        arguments = List(
          Argument("apiKey", apiKey, "String!"),
          Argument("ref", ref, "String!"),
          Argument("ifAfterId", ifAfterId, "ID")
        )
      )

  }
}
