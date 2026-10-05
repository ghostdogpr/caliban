package caliban.gateway

import caliban.InputValue
import caliban.Value.{ EnumValue, StringValue }
import caliban.gateway.internal.composition.FieldSelectionMap
import caliban.gateway.internal.composition.FieldSelectionMap._
import caliban.gateway.internal.composition.FieldSelectionMap.Selection._
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.Type
import caliban.parsing.parsers.Parsers
import caliban.tools.RemoteSchema
import zio.test._

// Examples from the Composite Schemas spec, Appendix A (graphql/composite-schemas-spec@dbf5cf9).
object FieldSelectionMapSpec extends ZIOSpecDefault {

  // Types for the examples of the Appendix A introduction and Language section, which show only the argument.
  private val products =
    """
      |type Query { product: Product book: Book mediaById(mediaId: ID): Media }
      |type Product {
      |  id: ID!
      |  weight: Float
      |  shippingWeight: Float
      |  packaging(material: Material): Packaging
      |  width: Float
      |  height: Float
      |  dimension: Dimension!
      |  size: Dimension
      |  dimensions: [Dimension]
      |  parts: [Part!]!
      |}
      |type Packaging { weight: Float }
      |type Dimension { size: Int width: Float height: Float weight: Int }
      |type Part { id: ID! name: String! }
      |type DeliveryEstimates { days: Int }
      |type User { id: ID! firstName: String }
      |interface Media { id: ID! }
      |type Book implements Media { id: ID! title: String! isbn: String! }
      |type Movie implements Media { id: ID! movieTitle: String! }
      |input DimensionInput { width: Float height: Float w: Float h: Float }
      |input PackageInput { weight: Float dimension: DimensionInput size: DimensionInput }
      |input UserInput { firstName: String }
      |input FindMediaInput @oneOf { bookId: ID movieId: ID }
      |input Nested { nested: FindMediaInput }
      |enum Material { BOX ENVELOPE }
      |scalar Currency
      |""".stripMargin

  // The type system of the Appendix A Validation section, with additions marked.
  private val media =
    """
      |type Query {
      |  mediaById(mediaId: ID!): Media
      |  findMedia(input: FindMediaInput): Media
      |  searchStore(search: SearchStoreInput): [Store]!
      |  storeById(id: ID!): Store
      |  nestedBookList: [[Book]] # added
      |}
      |type Store { id: ID! city: String! media: [Media!]! tags: [String!] } # tags added
      |interface Media { id: ID! }
      |type Book implements Media { id: ID! title: String! isbn: String! author: Author! }
      |type Movie implements Media { id: ID! movieTitle: String! releaseDate: String! }
      |type Author { id: ID! books: [Book!]! }
      |input FindMediaInput @oneOf { bookId: ID movieId: ID }
      |input SearchStoreInput { city: String hasInStock: FindMediaInput }
      |input Nested { nested: FindMediaInput }
      |input BookIdAndTitleInput { id: ID! title: String! } # added
      |""".stripMargin

  private def argument(
    sdl: String,
    typeName: String,
    fieldName: String,
    argumentName: String
  ): Either[String, List[String]] =
    for {
      document  <- Parser.parseQuery(sdl).left.map(_.msg)
      types     <- RemoteSchema.normalize(document).map(_.rootType.types).left.map(_.msg)
      parent    <- types.get(typeName).toRight(s"Missing type $typeName")
      field     <- fieldDefinition(parent, fieldName).toRight(s"Missing field $fieldName")
      argument  <- field.allArgs.find(_.name == argumentName).toRight(s"Missing argument $argumentName")
      directive <- argument.directives
                     .getOrElse(Nil)
                     .find(directive => directive.name == "is" || directive.name == "require")
                     .toRight("Missing @is or @require")
      map       <- stringArgument(directive.arguments, "field").toRight("Missing field argument")
      selected  <- FieldSelectionMap.parse(map)
      output     = if (directive.name == "is") field._type else parent
    } yield FieldSelectionMap.validate(selected, argument._type, output, types)

  private def require(sdl: String, argumentName: String): Either[String, List[String]] =
    argument(sdl, "Product", "shippingCost", argumentName)

  private def inContext(
    sdl: String,
    map: String,
    inputType: String,
    outputType: String
  ): Either[String, List[String]] =
    for {
      document <- Parser.parseQuery(sdl).left.map(_.msg)
      types    <- RemoteSchema.normalize(document).map(_.rootType.types).left.map(_.msg)
      input    <- typeReference(types, inputType)
      output   <- typeReference(types, outputType)
      selected <- FieldSelectionMap.parse(map)
    } yield FieldSelectionMap.validate(selected, input, output, types)

  private def typeReference(types: Map[String, __Type], reference: String): Either[String, __Type] = {
    def resolve(tpe: Type): Option[__Type] = {
      val resolved = tpe match {
        case Type.NamedType(name, _) => types.get(name)
        case Type.ListType(item, _)  => resolve(item).map(item => __Type(__TypeKind.LIST, ofType = Some(item)))
      }
      if (tpe.nonNull) resolved.map(tpe => __Type(__TypeKind.NON_NULL, ofType = Some(tpe))) else resolved
    }
    fastparse.parse(reference, Parsers.type_(_)).fold((_, _, _) => None, (tpe, _) => resolve(tpe)).toRight(reference)
  }

  private def leaf(field: String, arguments: (String, InputValue)*): Leaf =
    Leaf(::(PathStep(None, field, arguments.toMap), Nil))

  private def step(field: String): PathStep = PathStep(None, field, Map.empty)

  // Scala 2.12 assertTrue would type a direct FieldSelectionMap.parse call against a path-dependent existential.
  private def parsed(value: String): Either[String, SelectedValue] = FieldSelectionMap.parse(value)

  def spec = suite("FieldSelectionMapSpec")(
    suite("parsing")(
      test("reads every example of the Appendix A introduction and Language section") {
        val examples = List(
          "id",
          "{ firstName: firstName }",
          "dimension.size",
          "{ width: width, height: height }",
          "{ width, height }",
          "{ w: width, h: height }",
          "{ width: dimension.width, height: dimension.height }",
          "dimension.{ width, height }",
          "dimensions[{ width, height }]",
          "parts[id]",
          "{ weight, dimension: dimension.{ width, height } }",
          "{ weight, size: dimension.{ width, height } }",
          "{ weight, dimension: { width, height } }",
          "book.title",
          "{ width: width(unit: IMPERIAL), height: height(unit: IMPERIAL) }",
          "packaging(material: BOX).weight",
          "mediaById<Book>.isbn",
          "mediaById<Book>.title | mediaById<Movie>.movieTitle",
          "{ bookId: <Book>.id } | { movieId: <Movie>.id }",
          "{ nested: { bookId: <Book>.id } | { movieId: <Movie>.id } }",
          "dimensions[{ width(unit: IMPERIAL), height(unit: IMPERIAL) }]",
          "parts[{ id, name }]",
          "parts[[{ id, name }]]",
          "{ coordinates: coordinates[{ lat: x, lon: y }]}"
        )
        assertTrue(examples.filter(parsed(_).isLeft).isEmpty)
      },
      test("reads paths with arguments and type conditions") {
        val expected = Leaf(
          ::(
            PathStep(None, "packaging", Map("material" -> EnumValue("BOX"))),
            List(PathStep(Some("Book"), "isbn", Map.empty))
          )
        )
        assertTrue(
          parsed("packaging(material: BOX)<Book>.isbn") == Right(::[Selection](expected, Nil)),
          parsed("<Book>.title") == Right(
            ::[Selection](Leaf(::(PathStep(Some("Book"), "title", Map.empty), Nil)), Nil)
          )
        )
      },
      test("reads the shorthand object field as a path to the field of the same name") {
        val shorthand = parsed("{ width(unit: IMPERIAL) }")
        val explicit  = parsed("{ width: width(unit: IMPERIAL) }")
        assertTrue(
          shorthand == explicit,
          shorthand == Right(
            ::[Selection](
              ObjectOf(Nil, ::(ObjectField("width", ::(leaf("width", "unit" -> EnumValue("IMPERIAL")), Nil)), Nil)),
              Nil
            )
          )
        )
      },
      test("reads object and list selections after a path") {
        assertTrue(
          parsed("dimension.{ size }") ==
            Right(
              ::[Selection](ObjectOf(List(step("dimension")), ::(ObjectField("size", ::(leaf("size"), Nil)), Nil)), Nil)
            ),
          parsed("parts[[id]]") ==
            Right(::[Selection](ListOf(List(step("parts")), ::(ListOf(Nil, ::(leaf("id"), Nil)), Nil)), Nil))
        )
      },
      test("reads alternatives, with an optional leading |") {
        val alternatives: SelectedValue = ::(leaf("a"), List(leaf("b")))
        assertTrue(
          parsed("a | b") == Right(alternatives),
          parsed("| a | b") == Right(alternatives),
          parsed("{ x: a | b }") == Right(
            ::[Selection](ObjectOf(Nil, ::(ObjectField("x", alternatives), Nil)), Nil)
          )
        )
      },
      test("reads string, list and object constants, and optional commas") {
        assertTrue(
          parsed("""a(x: "s", y: [1, 2], z: { k: true }),""").map(_.head) ==
            Right(
              leaf(
                "a",
                "x" -> StringValue("s"),
                "y" -> InputValue.ListValue(List(caliban.Value.IntValue(1), caliban.Value.IntValue(2))),
                "z" -> InputValue.ObjectValue(Map("k" -> caliban.Value.BooleanValue(true)))
              )
            )
        )
      },
      test("rejects invalid syntax") {
        val invalid = List(
          "",
          "{}",
          "{ }",
          "a()",
          "a.",
          "a..b",
          "<Book>",
          "a<Book>",
          "a<Book>.{ b }",
          "parts[id, name]",
          "parts[]",
          "[id]",
          "{ a(x: 1): b }",
          "a(x: 1, x: 2)",
          "a |",
          "a b"
        )
        assertTrue(invalid.filter(parsed(_).isRight).isEmpty)
      },
      test("rejects variables") {
        assertTrue(
          parsed("width(unit: $unit)").isLeft,
          parsed("width(units: [$unit])").isLeft,
          parsed("width(unit: { value: $unit })").isLeft,
          parsed("{ width(unit: $unit) }").isLeft
        )
      }
    ),
    suite("introduction and Language examples")(
      test("@is maps a lookup argument to fields of the returned type") {
        val sdl = products +
          """
            |extend type Query {
            |  userById(userId: ID! @is(field: "id")): User!
            |  findUserByName(user: UserInput! @is(field: "{ firstName: firstName }")): User
            |  findUserByNameWithoutMapping(user: UserInput! @is(field: "firstName")): User
            |}
            |""".stripMargin
        assertTrue(
          argument(sdl, "Query", "userById", "userId") == Right(Nil),
          argument(sdl, "Query", "findUserByName", "user") == Right(Nil),
          argument(sdl, "Query", "findUserByNameWithoutMapping", "user") ==
            Right(List("Selected type 'String' does not match input type 'UserInput!'."))
        )
      },
      test("@require maps an argument to fields of the parent type") {
        val valid   = List(
          "weight: Float @require(field: \"weight\")",
          "weight: Float @require(field: \"shippingWeight\")",
          "weight: Float @require(field: \"packaging.weight\")",
          "weight: Float @require(field: \"packaging(material: BOX).weight\")",
          "dimension: DimensionInput @require(field: \"{ width: width, height: height }\")",
          "dimension: DimensionInput @require(field: \"{ width, height }\")",
          "dimension: DimensionInput @require(field: \"{ w: width, h: height }\")",
          "dimension: DimensionInput @require(field: \"{ width: dimension.width, height: dimension.height }\")",
          "dimension: DimensionInput @require(field: \"{ width: size.width, height: size.height }\")",
          "dimension: DimensionInput @require(field: \"dimension.{ width, height }\")",
          "dimension: DimensionInput @require(field: \"size.{ width, height }\")",
          "dimension: [DimensionInput] @require(field: \"dimensions[{ width, height }]\")",
          "dimension: [ID] @require(field: \"parts[id]\")",
          "dimension: PackageInput @require(field: \"{ weight, dimension: dimension.{ width, height } }\")",
          "dimension: PackageInput @require(field: \"{ weight, size: dimension.{ width, height } }\")",
          "dimension: PackageInput @require(field: \"{ weight, dimension: { width, height } }\")"
        )
        val results = valid.map(definition =>
          definition -> require(
            products + s"extend type Product { shippingCost($definition): Currency }",
            definition.takeWhile(_ != ':')
          )
        )
        assertTrue(results.filterNot(_._2 == Right(Nil)).isEmpty)
      },
      test("@require on several arguments of one field") {
        val sdl = products +
          """
            |extend type Product {
            |  delivery(
            |    zip: String!
            |    size: Int! @require(field: "dimension.size")
            |    weight: Int! @require(field: "dimension.weight")
            |  ): DeliveryEstimates
            |}
            |""".stripMargin
        assertTrue(
          argument(sdl, "Product", "delivery", "size") == Right(Nil),
          argument(sdl, "Product", "delivery", "weight") == Right(Nil)
        )
      },
      test("counter-examples: an input object needs an explicit mapping, lists need brackets") {
        def shippingCost(definition: String): Either[String, List[String]] =
          require(products + s"extend type Product { shippingCost($definition): Currency }", "dimension")
        assertTrue(
          shippingCost("dimension: DimensionInput @require(field: \"dimension\")") ==
            Right(List("Selected type 'Dimension!' must have subselections.")),
          shippingCost(
            "dimension: [DimensionInput] @require(field: \"{ width: dimensions.width, height: dimensions.height }\")"
          ) == Right(List("Input type '[DimensionInput]' is not an input object type."))
        )
      },
      test("paths with type conditions and alternatives") {
        assertTrue(
          inContext(products, "book.title", "String", "Query") == Right(Nil),
          inContext(products, "mediaById<Book>.isbn", "String", "Query") == Right(Nil),
          inContext(products, "mediaById<Book>.title | mediaById<Movie>.movieTitle", "String", "Query") == Right(Nil),
          inContext(products, "{ bookId: <Book>.id } | { movieId: <Movie>.id }", "FindMediaInput", "Media") ==
            Right(Nil),
          inContext(products, "{ nested: { bookId: <Book>.id } | { movieId: <Movie>.id } }", "Nested", "Media") ==
            Right(Nil)
        )
      },
      test("arguments on selected fields") {
        val sdl =
          """
            |type Query { product: Product }
            |type Product {
            |  width(unit: Unit!): Float!
            |  height(unit: Unit!): Float!
            |  dimensions: [Dimension]
            |  shippingCost(
            |    dimensions: DimensionInput
            |      @require(field: "{ width: width(unit: IMPERIAL), height: height(unit: IMPERIAL) }")
            |    # The spec types this argument DimensionInput, which a list selection cannot match.
            |    dimensionList: [DimensionInput]
            |      @require(field: "dimensions[{ width(unit: IMPERIAL), height(unit: IMPERIAL) }]")
            |  ): Currency
            |}
            |type Dimension { width(unit: Unit!): Float! height(unit: Unit!): Float! }
            |input DimensionInput { width: Float height: Float }
            |enum Unit { IMPERIAL METRIC }
            |scalar Currency
            |""".stripMargin
        assertTrue(require(sdl, "dimensions") == Right(Nil), require(sdl, "dimensionList") == Right(Nil))
      },
      test("object selections scoped by a path") {
        val sdl =
          """
            |type Query { product: Product }
            |type Product {
            |  dimension: Dimension!
            |  shippingCost(dimension: DimensionInput! @require(field: "dimension.{ size, weight }")): Int!
            |  shippingCostExplicit(
            |    dimension: DimensionInput! @require(field: "{ size: dimension.size, weight: dimension.weight }")
            |  ): Int!
            |}
            |type Dimension { size: Int! weight: Int! }
            |input DimensionInput { size: Int! weight: Int! }
            |""".stripMargin
        assertTrue(
          require(sdl, "dimension") == Right(Nil),
          argument(sdl, "Product", "shippingCostExplicit", "dimension") == Right(Nil)
        )
      },
      test("list selections") {
        val sdl =
          """
            |type Query { product: Product findLocation(location: LocationInput! @is(field: "{ coordinates: coordinates[{ lat: x, lon: y }]}")): Location }
            |type Product {
            |  parts: [Part!]!
            |  nestedParts: [[Part!]]!
            |  partIds(partIds: [ID!]! @require(field: "parts[id]")): [ID!]!
            |  partInputs(parts: [PartInput!]! @require(field: "parts[{ id, name }]")): [ID!]!
            |  nestedPartInputs(parts: [[PartInput!]]! @require(field: "nestedParts[[{ id, name }]]")): [ID!]!
            |}
            |type Part { id: ID! name: String! }
            |input PartInput { id: ID! name: String! }
            |type Coordinate { x: Int! y: Int! }
            |type Location { coordinates: [Coordinate!]! }
            |input PositionInput { lat: Int! lon: Int! }
            |input LocationInput { coordinates: [PositionInput!]! }
            |""".stripMargin
        assertTrue(
          argument(sdl, "Product", "partIds", "partIds") == Right(Nil),
          argument(sdl, "Product", "partInputs", "parts") == Right(Nil),
          argument(sdl, "Product", "nestedPartInputs", "parts") == Right(Nil),
          argument(sdl, "Query", "findLocation", "location") == Right(Nil)
        )
      }
    ),
    suite("validation rules")(
      test("Path Field Selections") {
        assertTrue(
          inContext(media, "title", "String", "Book") == Right(Nil),
          inContext(media, "<Book>.title", "String", "Book") == Right(Nil),
          inContext(media, "movieId", "ID", "Book") == Right(List("Field 'movieId' is not defined on type 'Book'.")),
          inContext(media, "<Book>.movieId", "ID", "Book") == Right(
            List("Field 'movieId' is not defined on type 'Book'.")
          ),
          inContext(media, "<Book>.movieId", "ID", "Media") == Right(
            List("Field 'movieId' is not defined on type 'Book'.")
          ),
          inContext(media, "media.id", "ID", "Store") ==
            Right(List("Field 'id' cannot be selected on list type '[Media!]!'."))
        )
      },
      test("Path Field Argument Validity") {
        val sdl =
          """
            |type Query { product: Product }
            |type Product {
            |  width(unit: Unit!): Float!
            |  depth(unit: Unit!, scale: Float! @require(field: "scale")): Float!
            |  scale: Float!
            |  shippingCost(
            |    valid: Float @require(field: "width(unit: IMPERIAL)")
            |    unknown: Float @require(field: "width(scale: IMPERIAL)")
            |    missing: Float @require(field: "width")
            |    incompatible: Float @require(field: "width(unit: 5)")
            |    requiredByRequire: Float @require(field: "depth(unit: IMPERIAL)")
            |  ): Currency
            |}
            |enum Unit { IMPERIAL METRIC }
            |scalar Currency
            |""".stripMargin
        assertTrue(
          require(sdl, "valid") == Right(Nil),
          require(sdl, "unknown") == Right(
            List(
              "Unknown argument 'scale' on field 'Product.width'.",
              "Required argument 'unit' is missing on field 'Product.width'."
            )
          ),
          require(sdl, "missing") == Right(List("Required argument 'unit' is missing on field 'Product.width'.")),
          require(sdl, "incompatible") == Right(List("Argument 'unit' on field 'Product.width' has invalid type: 5")),
          require(sdl, "requiredByRequire") == Right(Nil),
          inContext(media, "mediaById<Book>.isbn", "String", "Query") ==
            Right(List("Required argument 'mediaId' is missing on field 'Query.mediaById'.")),
          inContext(media, "mediaById(mediaId: \"1\")<Book>.isbn", "String", "Query") == Right(Nil)
        )
      },
      test("Path Terminal Field Selections") {
        assertTrue(
          inContext(media, "author.id", "ID", "Book") == Right(Nil),
          inContext(media, "title.something", "String", "Book") ==
            Right(List("Field 'something' cannot be selected on leaf type 'String!'.")),
          inContext(media, "author", "ID", "Book") == Right(List("Selected type 'Author!' must have subselections.")),
          inContext(media, "title.{ id }", "BookIdAndTitleInput", "Book") == Right(
            List(
              "Required input field 'title' is not selected.",
              "Field 'id' cannot be selected on leaf type 'String!'."
            )
          )
        )
      },
      test("Type Reference Is Possible") {
        assertTrue(
          inContext(media, "<Book>.id", "ID", "Media") == Right(Nil),
          inContext(media, "<Media>.id", "ID", "Book") == Right(Nil),
          inContext(media, "findMedia<Book>.isbn", "String", "Query") == Right(Nil),
          inContext(media, "findMedia<Store>.id", "ID", "Query") ==
            Right(List("Type 'Store' is not a possible type of 'Media'.")),
          inContext(media, "<Movie>.id", "ID", "Book") == Right(List("Type 'Movie' is not a possible type of 'Book'.")),
          inContext(media, "<Missing>.id", "ID", "Media") ==
            Right(List("Type 'Missing' is not a possible type of 'Media'."))
        )
      },
      test("Values of Correct Type") {
        def store(idType: String): String =
          s"""
             |type Query { storeById(id: ID! @is(field: "id")): Store! }
             |type Store { id: $idType city: String! }
             |""".stripMargin
        assertTrue(
          argument(store("ID"), "Query", "storeById", "id") == Right(Nil),
          argument(store("Int"), "Query", "storeById", "id") ==
            Right(List("Selected type 'Int' does not match input type 'ID!'.")),
          inContext(media, "storeById(id: 1).media[id]", "[ID]", "Query") == Right(Nil),
          inContext(media, "nestedBookList[[id]]", "[[ID]]", "Query") == Right(Nil),
          inContext(media, "tags", "[String]", "Store") == Right(Nil),
          inContext(media, "tags", "[ID]", "Store") ==
            Right(List("Selected type '[String!]' does not match input type '[ID]'.")),
          inContext(media, "tags", "String", "Store") ==
            Right(List("Selected type '[String!]' does not match input type 'String'.")),
          inContext(media, "media[id]", "ID", "Store") ==
            Right(List("List selection needs list types, found input type 'ID' and selected type '[Media!]!'.")),
          inContext(media, "city[id]", "[ID]", "Store") ==
            Right(List("List selection needs list types, found input type '[ID]' and selected type 'String!'.")),
          inContext(media, "nestedBookList[[{ id, title }]]", "[[[BookIdAndTitleInput]]]", "Query") ==
            Right(List("Input type '[BookIdAndTitleInput]' is not an input object type.")),
          inContext(media, "{ id }", "ID", "Book") == Right(List("Input type 'ID' is not an input object type."))
        )
      },
      test("Selected Object Field Names") {
        val sdl =
          """
            |type Query { storeById(id: ID! @is(field: "id")): Store! storeByAddress(id: ID! @is(field: "address")): Store! }
            |type Store { id: ID city: String! }
            |""".stripMargin
        assertTrue(
          argument(sdl, "Query", "storeById", "id") == Right(Nil),
          // The spec lists this example under this rule, but "address" is a path, not an object field.
          argument(sdl, "Query", "storeByAddress", "id") == Right(
            List("Field 'address' is not defined on type 'Store'.")
          ),
          inContext(media, "{ id, title, isbn }", "BookIdAndTitleInput", "Book") ==
            Right(List("Field 'isbn' is not defined on input type 'BookIdAndTitleInput'."))
        )
      },
      test("Selected Object Field Uniqueness") {
        assertTrue(
          inContext(media, "{ id, id }", "ID!", "Store") == Right(
            List("Input field 'id' is selected more than once.", "Input type 'ID!' is not an input object type.")
          ),
          inContext(media, "{ id, title, id: author.id }", "BookIdAndTitleInput", "Book") ==
            Right(List("Input field 'id' is selected more than once."))
        )
      },
      test("Required Selected Object Fields") {
        val sdl =
          """
            |type Query {
            |  userById(user: UserInput! @is(field: "{ name: name }")): User!
            |  findUser(input: FindUserInput! @is(field: "{ name: name }")): User!
            |  findUserById(input: FindUserByIdInput! @is(field: "{ id: id }")): User!
            |}
            |type User { id: ID name: String! }
            |input UserInput { id: ID! name: String! }
            |input FindUserInput { id: ID name: String }
            |input FindUserByIdInput { id: ID name: String! }
            |""".stripMargin
        assertTrue(
          argument(sdl, "Query", "userById", "user") == Right(List("Required input field 'id' is not selected.")),
          argument(sdl, "Query", "findUser", "input") == Right(Nil),
          argument(sdl, "Query", "findUserById", "input") == Right(
            List("Required input field 'name' is not selected.")
          ),
          inContext(media, "{ bookId: <Book>.id } | { movieId: <Movie>.id }", "FindMediaInput", "Media") == Right(Nil),
          inContext(media, "{ bookId: <Book>.id, movieId: <Movie>.id }", "FindMediaInput", "Media") ==
            Right(List("Selection on oneOf input type 'FindMediaInput' must select one field."))
        )
      }
    )
  )
}
