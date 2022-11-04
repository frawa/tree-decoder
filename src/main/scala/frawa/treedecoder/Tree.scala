package frawa.treedecoder

trait TreeData[Tree, D]:
  extension (node: Tree) def data(): D

trait TreeFinder[Tree]:
  extension (parent: Tree) def children(): Seq[Tree]

extension [Tree](parent: Tree)
  def findChild[D](data: D)(using TreeFinder[Tree])(using TreeData[Tree, D]): Option[Tree] =
    TreeFinder.findChild(parent, data)

object TreeFinder:
  def findChild[Tree, D](node: Tree, data: D)(using TreeFinder[Tree])(using TreeData[Tree, D]): Option[Tree] =
    node
      .children()
      .filter(_.data() == data)
      .headOption
      .orElse(node.children().flatMap(findChild(_, data)).headOption)
