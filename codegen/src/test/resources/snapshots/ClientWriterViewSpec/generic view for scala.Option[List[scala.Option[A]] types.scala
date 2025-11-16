import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type ProjectMember
  object ProjectMember {

    final case class ProjectMemberView(id: scala.Option[Int], name: scala.Option[String])

    type ViewSelection = SelectionBuilder[ProjectMember, ProjectMemberView]

    def view: ViewSelection = (id ~ name).map { case (id, name) => ProjectMemberView(id, name) }

    def id: SelectionBuilder[ProjectMember, scala.Option[Int]]      =
      _root_.caliban.client.SelectionBuilder.Field("id", OptionOf(Scalar()))
    def name: SelectionBuilder[ProjectMember, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("name", OptionOf(Scalar()))
  }

  type ProjectMemberEdge
  object ProjectMemberEdge {

    final case class ProjectMemberEdgeView[NodeSelection](cursor: String, node: scala.Option[NodeSelection])

    type ViewSelection[NodeSelection] = SelectionBuilder[ProjectMemberEdge, ProjectMemberEdgeView[NodeSelection]]

    def view[NodeSelection](
      nodeSelection: SelectionBuilder[ProjectMember, NodeSelection]
    ): ViewSelection[NodeSelection] = (cursor ~ node(nodeSelection)).map { case (cursor, node) =>
      ProjectMemberEdgeView(cursor, node)
    }

    def cursor: SelectionBuilder[ProjectMemberEdge, String] =
      _root_.caliban.client.SelectionBuilder.Field("cursor", Scalar())
    def node[A](
      innerSelection: SelectionBuilder[ProjectMember, A]
    ): SelectionBuilder[ProjectMemberEdge, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("node", OptionOf(Obj(innerSelection)))
  }

  type PageInfo
  object PageInfo {

    final case class PageInfoView(
      endCursor: scala.Option[String],
      hasNextPage: Boolean,
      hasPreviousPage: Boolean,
      startCursor: scala.Option[String]
    )

    type ViewSelection = SelectionBuilder[PageInfo, PageInfoView]

    def view: ViewSelection = (endCursor ~ hasNextPage ~ hasPreviousPage ~ startCursor).map {
      case (endCursor, hasNextPage, hasPreviousPage, startCursor) =>
        PageInfoView(endCursor, hasNextPage, hasPreviousPage, startCursor)
    }

    def endCursor: SelectionBuilder[PageInfo, scala.Option[String]]   =
      _root_.caliban.client.SelectionBuilder.Field("endCursor", OptionOf(Scalar()))
    def hasNextPage: SelectionBuilder[PageInfo, Boolean]              =
      _root_.caliban.client.SelectionBuilder.Field("hasNextPage", Scalar())
    def hasPreviousPage: SelectionBuilder[PageInfo, Boolean]          =
      _root_.caliban.client.SelectionBuilder.Field("hasPreviousPage", Scalar())
    def startCursor: SelectionBuilder[PageInfo, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("startCursor", OptionOf(Scalar()))
  }

  type ProjectMemberConnection
  object ProjectMemberConnection {

    final case class ProjectMemberConnectionView[EdgesSelection, NodesSelection, PageInfoSelection](
      edges: scala.Option[List[scala.Option[EdgesSelection]]],
      nodes: scala.Option[List[scala.Option[NodesSelection]]],
      pageInfo: PageInfoSelection
    )

    type ViewSelection[EdgesSelection, NodesSelection, PageInfoSelection] = SelectionBuilder[
      ProjectMemberConnection,
      ProjectMemberConnectionView[EdgesSelection, NodesSelection, PageInfoSelection]
    ]

    def view[EdgesSelection, NodesSelection, PageInfoSelection](
      edgesSelection: SelectionBuilder[ProjectMemberEdge, EdgesSelection],
      nodesSelection: SelectionBuilder[ProjectMember, NodesSelection],
      pageInfoSelection: SelectionBuilder[PageInfo, PageInfoSelection]
    ): ViewSelection[EdgesSelection, NodesSelection, PageInfoSelection] =
      (edges(edgesSelection) ~ nodes(nodesSelection) ~ pageInfo(pageInfoSelection)).map {
        case (edges, nodes, pageInfo) => ProjectMemberConnectionView(edges, nodes, pageInfo)
      }

    def edges[A](
      innerSelection: SelectionBuilder[ProjectMemberEdge, A]
    ): SelectionBuilder[ProjectMemberConnection, scala.Option[List[scala.Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("edges", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
    def nodes[A](
      innerSelection: SelectionBuilder[ProjectMember, A]
    ): SelectionBuilder[ProjectMemberConnection, scala.Option[List[scala.Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[ProjectMemberConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

}
