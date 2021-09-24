package poc.caliban.posts

import caliban.GraphQL.graphQL
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers._
import caliban.{GraphQL, RootResolver}
import zio._
import zio.duration.durationInt
import zio.stream.ZStream

object Operations {

  final case class CreatePostMutationParams(authorName: AuthorName, title: PostTitle, content: PostContent)

  final case class Query(
    postById: PostId => ZIO[Has[PostService], PostServiceError, Option[Post]]
  )

  final case class Mutation(
    createPost: CreatePostMutationParams => ZIO[Has[PostService], PostServiceError, Post],
    deletePost: PostId => ZIO[Has[PostService], PostServiceError, Unit]
  )

  final case class Subscription(
    allPostsByAuthor: AuthorName => ZStream[Has[PostService], PostServiceError, Post]
  )
}

object Resolvers {
  import Operations._

  private val queries =
    Query(
      postById = id => PostService(_.findById(id))
    )

  private val mutations =
    Mutation(
      createPost = args => PostService(_.createPost(args.authorName, args.title, args.content)),
      deletePost = id => PostService(_.deletePost(id))
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

object Schemas extends GenericSchema[ZEnv with Has[PostService]]

object GraphQLApi {
  import Schemas._

  val api: GraphQL[ZEnv with Has[PostService]] =
    graphQL(
      Resolvers.resolver
    ) @@
      maxFields(200) @@
      maxDepth(30) @@
      timeout(5.seconds) @@
      printSlowQueries(500.millis) @@
      printErrors

}
