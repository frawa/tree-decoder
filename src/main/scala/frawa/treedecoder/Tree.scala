package frawa.treedecoder

trait Tree[Node, Data]:
  extension (node: Node) def data(): Data
  extension (parent: Node) def children(): Seq[Node]

extension [Node, Data](parent: Node)
  def findChild(data: Data)(using Tree[Node, Data]): Option[Node] =
    TreeFinder.findChild(parent, data)

object TreeFinder:
  def findChild[Node, Data](node: Node, data: Data)(using Tree[Node, Data]): Option[Node] =
    node
      .children()
      .filter(_.data() == data)
      .headOption
      .orElse(node.children().flatMap(findChild(_, data)).headOption)
