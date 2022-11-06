package frawa.treedecoder

trait Tree[Node, Data]:
  extension (node: Node) def data: Data
  extension (parent: Node) def children: Seq[Node]

extension [Node, Data](node: Node)
  def find(data: Data)(using Tree[Node, Data]): Seq[Node] =
    TreeFinder.find(Seq(node), data)

object TreeFinder:
  def find[Node, Data](at: Seq[Node], data: Data)(using Tree[Node, Data]): Seq[Node] =
    at.headOption
      .map { node =>
        if node.data == data
        then node +: at.drop(1)
        else
          node.children
            .filter(_.data == data)
            .headOption
            .map(_ +: at)
            .orElse {
              node.children
                .map(ch => find(Seq(ch), data))
                .filterNot(_.isEmpty)
                .headOption
                .map(_ ++ at)
            }
            .getOrElse(find(nextAfter(at), data))
      }
      .getOrElse(Seq())

  def nextAfter[Node, Data](at: Seq[Node])(using
      Tree[Node, Data]
  ): Seq[Node] =
    val parents = at.drop(1)
    val parent  = parents.headOption
    val node    = at.headOption
    node
      .zip(parent)
      .flatMap { (node, parent) =>
        parent.children
          .dropWhile(!_.asInstanceOf[AnyRef].equals(node.asInstanceOf[AnyRef]))
          .drop(1)
          .headOption
          .map(_ +: parents)
      }
      .getOrElse(if parents.nonEmpty then nextAfter(parents) else Seq())
