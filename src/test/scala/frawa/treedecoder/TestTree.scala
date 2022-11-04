package frawa.treedecoder

enum TestTree:
  case Leaf(text: String)
  case Node(title: String, children: Seq[TestTree] = Seq())

given Tree[TestTree, String] with
  extension (node: TestTree)
    def data(): String = node match {
      case TestTree.Leaf(text)            => text
      case TestTree.Node(title, children) => title
    }
    def children(): Seq[TestTree] =
      node match {
        case _: TestTree.Leaf           => Seq()
        case TestTree.Node(_, children) => children
      }
