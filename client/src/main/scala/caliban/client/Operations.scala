package caliban.client

object Operations {
  type RootQuery
  type RootMutation
  type RootSubscription

  /**
   * Typeclass used to enforce that we can only create a request for one of the root fields.
   */
  trait IsOperation[A] {
    def operationName: String
  }

  object IsOperation {
    implicit val query: IsOperation[RootQuery] = new IsOperation[RootQuery] {
      override def operationName: String = "query"
    }
    implicit val mutation: IsOperation[RootMutation] = new IsOperation[RootMutation] {
      override def operationName: String = "mutation"
    }
    implicit val subscription: IsOperation[RootSubscription] = new IsOperation[RootSubscription] {
      override def operationName: String = "subscription"
    }
  }
}
