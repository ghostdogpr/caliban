package poc.caliban.posts

import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers._
import caliban.{GraphQL, RootResolver}
import zio._
import zio.stream.ZStream

import scala.language.higherKinds

object Operations {

  final case class CreatePostMutationParams(authorName: AuthorName, title: PostTitle, content: PostContent)

  final case class Query(
    postById: PostId => ZIO[PostService, PostServiceError, Post]
  )

  final case class Mutation(
    createPost: CreatePostMutationParams => ZIO[PostService, PostServiceError, Post],
    deletePost: PostId => ZIO[PostService, PostServiceError, Unit]
  )

  final case class Subscription(
    allPostsByAuthor: AuthorName => ZStream[PostService, PostServiceError, Post]
  )
}

object Resolvers {
  import Operations._

  private val queries =
    Query(
      postById = id => PostService.findById(id)
    )

  private val mutations =
    Mutation(
      createPost = args => PostService.createPost(args.authorName, args.title, args.content),
      deletePost = id => PostService.deletePost(id)
    )

  private val subscriptions =
    Subscription(
      allPostsByAuthor = author =>
        ZStream
          .service[PostService]
          .flatMap(_.all.filter(_.author == author))
    )

  val resolver: RootResolver[Query, Mutation, Subscription] = RootResolver(queries, mutations, subscriptions)
}

object Schemas extends GenericSchema[PostService]

object GraphQLApi {
  import Schemas._

  val api: GraphQL[PostService] =
    graphQL[PostService, Operations.Query, Operations.Mutation, Operations.Subscription](
      Resolvers.resolver
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors

}
