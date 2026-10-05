package caliban.gateway

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.{ GQLDefault, GQLDirective, GQLName, GQLValueType }
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.{ graphQL, GraphQL, RootResolver }
import zio.{ UIO, ZIO }

// The eShop source schemas of the ChilliCream composite-schema benchmark as local Caliban subgraphs, with the
// benchmark's data, and the same data served by one monolithic API to compare results against.
private[gateway] object CompositeEShop {

  final class key(fields: String)    extends GQLDirective(Directive("key", Map("fields" -> StringValue(fields))))
  final class lookup                 extends GQLDirective(Directive("lookup"))
  final class internal               extends GQLDirective(Directive("internal"))
  final class require(field: String) extends GQLDirective(Directive("require", Map("field" -> StringValue(field))))

  @GQLValueType(isScalar = true)
  @GQLName("ID")
  final case class ID(value: String)
  final case class ByID(id: ID)
  final case class ByUpc(upc: ID)
  final case class Top(@GQLDefault("5") first: Int)

  implicit val idSchema: Schema[Any, ID] = Schema.gen
  implicit val idArg: ArgBuilder[ID]     = ArgBuilder.string.map(ID(_))
  implicit val byId: ArgBuilder[ByID]    = ArgBuilder.gen
  implicit val byUpc: ArgBuilder[ByUpc]  = ArgBuilder.gen
  implicit val top: ArgBuilder[Top]      = ArgBuilder.gen

  private final case class UserRow(id: String, name: String, username: String)
  private final case class ProductRow(upc: String, name: String, price: Long, weight: Long, inStock: Boolean)
  private final case class ReviewRow(id: String, body: String, authorId: String, productUpc: String)

  private val users = List(
    UserRow("1", "Uri Goldshtein", "urigo"),
    UserRow("2", "Dotan Simha", "dotansimha"),
    UserRow("3", "Kamil Kisiela", "kamilkisiela"),
    UserRow("4", "Arda Tanrikulu", "ardatan"),
    UserRow("5", "Gil Gardosh", "gilgardosh"),
    UserRow("6", "Laurin Quast", "laurin")
  )

  private val products = List(
    ProductRow("1", "Table", 899, 100, inStock = true),
    ProductRow("2", "Couch", 1299, 1000, inStock = false),
    ProductRow("3", "Glass", 15, 20, inStock = false),
    ProductRow("4", "Chair", 499, 100, inStock = false),
    ProductRow("5", "TV", 1299, 1000, inStock = true),
    ProductRow("6", "Lamp", 6999, 300, inStock = true),
    ProductRow("7", "Grill", 3999, 2000, inStock = true),
    ProductRow("8", "Fridge", 100000, 6000, inStock = false),
    ProductRow("9", "Sofa", 9999, 800, inStock = true)
  )

  private val reviews = List(
    ReviewRow("1", "Review 1", "1", "1"),
    ReviewRow("2", "Review 2", "1", "1"),
    ReviewRow("3", "Review 3", "1", "1"),
    ReviewRow("4", "Review 4", "1", "1"),
    ReviewRow("5", "Review 5", "1", "2"),
    ReviewRow("6", "Review 6", "1", "2"),
    ReviewRow("7", "Review 7", "1", "2"),
    ReviewRow("8", "Review 8", "1", "2"),
    ReviewRow("9", "Review 9", "1", "3"),
    ReviewRow("10", "Review 10", "1", "4"),
    ReviewRow("11", "Review 11", "1", "4")
  )

  // As in the benchmark, every user's reviews are the first two.
  private val userReviews = reviews.take(2)

  object Accounts extends GenericSchema[Any] {
    import auto._

    @key("id")
    final case class User(id: ID, name: Option[String], username: Option[String], birthday: Option[Int])
    final case class Query(me: Option[User], @lookup user: ByID => Option[User], users: List[User])

    private def user(row: UserRow) = User(ID(row.id), Some(row.name), Some(row.username), Some(1234567890))

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          users.headOption.map(user),
          args => users.find(_.id == args.id.value).map(user),
          users.map(user)
        )
      )
    )
  }

  // As in the benchmark's inventory subgraph.
  private def shippingEstimate(weight: Long, price: Long): Long = if (price > 1000) 0 else weight / 2

  object Inventory extends GenericSchema[Any] {
    import auto._

    final case class Estimate(@require("weight") weight: Long, @require("price") price: Long)
    @key("upc")
    final case class Product(upc: String, inStock: Boolean, shippingEstimate: Estimate => Option[Long])
    final case class Query(@lookup @internal productByUpc: ByUpc => Option[Product])

    implicit val estimate: ArgBuilder[Estimate] = ArgBuilder.gen

    private def product(row: ProductRow) =
      Product(row.upc, row.inStock, args => Some(CompositeEShop.shippingEstimate(args.weight, args.price)))

    val api: GraphQL[Any] = graphQL(RootResolver(Query(args => products.find(_.upc == args.upc.value).map(product))))
  }

  object Products extends GenericSchema[Any] {
    import auto._

    @key("upc")
    final case class Product(upc: String, name: String, price: Long, weight: Long)
    final case class Query(topProducts: Top => List[Product], @lookup product: ByUpc => Option[Product])

    private def product(row: ProductRow) = Product(row.upc, row.name, row.price, row.weight)

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          args => products.take(args.first).map(product),
          args => products.find(_.upc == args.upc.value).map(product)
        )
      )
    )
  }

  object Reviews extends GenericSchema[Any] {
    import auto._

    @key("upc")
    final case class Product(reviews: UIO[List[Review]], upc: String)
    final case class Review(
      id: ID,
      author: User,
      product: Product,
      body: String,
      authorId: String,
      productUpc: String
    )
    @key("id")
    final case class User(id: ID, reviews: UIO[List[Review]])

    // A hand-written instance breaks the type cycle, which derivation cannot follow on both Scala versions.
    implicit lazy val reviewSchema: Schema[Any, Review] =
      obj("Review", directives = List(new key("id").directive))(implicit attributes =>
        List(
          field("id")(_.id),
          field("author")(_.author),
          field("product")(_.product),
          field("body")(_.body),
          field("authorId")(_.authorId),
          field("productUpc")(_.productUpc)
        )
      )

    final case class Query(
      @lookup @internal product: ByUpc => Product,
      @lookup review: ByID => Option[Review],
      @lookup @internal user: ByID => User
    )

    private def product(upc: String): Product  =
      Product(ZIO.succeed(reviews.filter(_.productUpc == upc).map(review)), upc)
    private def user(id: String): User         = User(ID(id), ZIO.succeed(userReviews.map(review)))
    private def review(row: ReviewRow): Review =
      Review(
        ID(row.id),
        user(row.authorId),
        product(row.productUpc),
        row.body,
        row.authorId,
        row.productUpc
      )

    val api: GraphQL[Any] = graphQL(
      RootResolver(
        Query(
          args => product(args.upc.value),
          args => reviews.find(_.id == args.id.value).map(review),
          args => user(args.id.value)
        )
      )
    )
  }

  object Monolith extends GenericSchema[Any] {
    import auto._

    final case class User(
      id: ID,
      name: Option[String],
      username: Option[String],
      birthday: Option[Int],
      reviews: UIO[List[Review]]
    )
    final case class Product(
      upc: String,
      name: String,
      price: Long,
      weight: Long,
      inStock: Boolean,
      shippingEstimate: Option[Long],
      reviews: UIO[List[Review]]
    )
    final case class Review(id: ID, body: String, author: Option[User], product: Option[Product])
    final case class Query(users: List[User], topProducts: Top => List[Product])

    implicit lazy val reviewSchema: Schema[Any, Review] =
      obj("Review")(implicit attributes =>
        List(field("id")(_.id), field("body")(_.body), field("author")(_.author), field("product")(_.product))
      )

    private def user(row: UserRow): User          =
      User(ID(row.id), Some(row.name), Some(row.username), Some(1234567890), ZIO.succeed(userReviews.map(review)))
    private def product(row: ProductRow): Product =
      Product(
        row.upc,
        row.name,
        row.price,
        row.weight,
        row.inStock,
        Some(shippingEstimate(row.weight, row.price)),
        ZIO.succeed(reviews.filter(_.productUpc == row.upc).map(review))
      )
    private def review(row: ReviewRow): Review    =
      Review(
        ID(row.id),
        row.body,
        users.find(_.id == row.authorId).map(user),
        products.find(_.upc == row.productUpc).map(product)
      )

    val api: GraphQL[Any] = graphQL(
      RootResolver(Query(users.map(user), args => products.take(args.first).map(product)))
    )
  }

  // The benchmark's k6 query.
  val query: String =
    """fragment User on User { id username name }
      |fragment Review on Review { id body }
      |fragment Product on Product { inStock name price shippingEstimate upc weight }
      |query TestQuery {
      |  users {
      |    ...User
      |    reviews {
      |      ...Review
      |      product {
      |        ...Product
      |        reviews { ...Review author { ...User reviews { ...Review product { ...Product } } } }
      |      }
      |    }
      |  }
      |  topProducts {
      |    ...Product
      |    reviews { ...Review author { ...User reviews { ...Review product { ...Product } } } }
      |  }
      |}""".stripMargin
}
