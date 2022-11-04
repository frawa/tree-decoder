package frawa.treedecoder

import munit.*

enum TestTree:
  case Leaf(text: String)
  case Node(title: String, children: Seq[TestTree] = Seq())

given TreeData[TestTree, String] with
  extension (node: TestTree)
    def data(): String = node match {
      case TestTree.Leaf(text)            => text
      case TestTree.Node(title, children) => title
    }

given TreeFinder[TestTree] with
  extension (parent: TestTree)
    def children(): Seq[TestTree] =
      parent match {
        case _: TestTree.Leaf           => Seq()
        case TestTree.Node(_, children) => children
      }

class TreeTest extends FunSuite {

  test("empty") {
    val emptyRoot = TestTree.Node("root")
    assertEquals(emptyRoot.data(), "root");
    assertEquals(emptyRoot.findChild("root"), None);
  }

  test("find child") {
    val root = TestTree.Node("root", Seq(TestTree.Leaf("foo")))
    assertEquals(root.data(), "root");
    assertEquals(root.findChild("foo").map(_.data()), Some("foo"));
  }

  test("find deep child") {
    val root = TestTree.Node("root", Seq(TestTree.Node("nested", Seq(TestTree.Leaf("foo")))))
    assertEquals(root.data(), "root");
    assertEquals(root.findChild("foo").map(_.data()), Some("foo"));
  }
}
