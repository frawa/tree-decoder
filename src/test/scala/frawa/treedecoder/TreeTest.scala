package frawa.treedecoder

import munit.*

class TreeTest extends FunSuite {
  import TestTree.*

  test("empty") {
    val emptyRoot = Node("root")
    assertEquals(emptyRoot.data(), "root");
    assertEquals(emptyRoot.findChild("root"), None);
  }

  test("find child") {
    val root = Node("root", Seq(Leaf("foo")))
    assertEquals(root.data(), "root");
    assertEquals(root.findChild("foo").map(_.data()), Some("foo"));
  }

  test("find deep child") {
    val root = Node("root", Seq(Node("nested", Seq(Leaf("foo")))))
    assertEquals(root.data(), "root");
    assertEquals(root.findChild("foo").map(_.data()), Some("foo"));
  }
}
