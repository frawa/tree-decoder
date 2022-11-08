package frawa.treedecoder

trait Tree[Node, Data]:
  extension (node: Node) def data: Data
  extension (parent: Node) def children: Seq[Node]

extension [Node, Data](node: Node)
  def find(data: Data)(using Tree[Node, Data]): TreeFinder.At[Node] =
    TreeFinder.find(TreeFinder.At.root(node), data)

object TreeFinder:
  opaque type At[Node] = Seq[At.Item[Node]]

  extension [Node](at: At[Node])
    def valid: Boolean                              = at.nonEmpty
    def withoutParent: At[Node]                     = at.headOption.map(At.root(_)).getOrElse(Seq())
    def withSameParentAs(other: At[Node]): At[Node] = at ++ other.drop(1)
    def asRoot: At[Node] = at.headOption
      .map(head => At.root(head.copy(nextSibling = None)))
      .getOrElse(Seq())
    def map[T](f: Node => T): Seq[T]                   = at.map(_.node).map(f)
    def mapNode[T](f: Node => T): Option[T]            = at.headOption.map(_.node).map(f)
    def dataPath[Data](using Tree[Node, Data]): String = at.map(_.node.data).reverse.mkString(".")

  object At:
    case class Item[Node](node: Node, nextSibling: Option[Item[Node]])

    def root[Node](node: Node): At[Node]                                = Seq(Item(node, None))
    def root[Node](item: Item[Node]): At[Node]                          = Seq(item)
    def withParent[Node](item: Item[Node], parents: At[Node]): At[Node] = item +: parents
    def siblings[Node](children: Seq[Node]): Seq[Item[Node]] =
      def children1 = children.map(Some(_))
      def siblings1 =
        if children.size > 1 then At.siblings(children.drop(1)).map(Some(_))
        else Seq()
      children1.zipAll(siblings1, None, None).map { case (child, sibling) =>
        Item(child.get, sibling)
      }

  def find[Node, Data](at: At[Node], data: Data)(using Tree[Node, Data]): At[Node] =
    at.mapNode { node =>
      if node.data == data
      then at
      else
        val children = At.siblings(node.children)
        children
          .filter(_.node.data == data)
          .headOption
          .map(n => At.withParent(n, at))
          .orElse {
            children
              .map(ch => find(At.root(ch), data))
              .filterNot(_.isEmpty)
              .headOption
              .map(_ ++ at)
          }
          .getOrElse(find(nextAfter(at), data))
    }.getOrElse(Seq())

  def nextAfter[Node, Data](at: At[Node])(using
      Tree[Node, Data]
  ): At[Node] =
    val parents = at.drop(1)
    at.headOption
      .flatMap(_.nextSibling)
      .map(n => At.withParent(n, parents))
      .getOrElse(if parents.nonEmpty then nextAfter(parents) else Seq())

end TreeFinder
