package frawa.treedecoder

import munit.*

import TreeFinder.*

class TreeTest extends FunSuite {
  import TestTree.*

  test("empty") {
    val emptyRoot = Node("root")
    assertEquals(emptyRoot.data, "root");
    assertEquals(emptyRoot.find("root"), At.root(emptyRoot));
  }

  test("find child") {
    val root = Node("root", Seq(Leaf("foo")))
    assertEquals(root.data, "root")
    assertEquals(root.find("foo").map(_.data), Seq("foo", "root"));
  }

  test("find deep child") {
    val root =
      Node("root", Seq(Node("another", Seq(Leaf("bar"))), Node("nested", Seq(Leaf("foo")))))
    assertEquals(root.data, "root")
    assertEquals(root.find("foo").map(_.data), Seq("foo", "nested", "root"));
  }

  test("next after") {
    val root = Node("root", Seq(Node("nested", Seq(Leaf("foo"))), Node("nested", Seq(Leaf("bar")))))
    val nested1 = root.find("nested")
    assertEquals(nested1.map(_.data), Seq("nested", "root"))
    assertEquals(nested1.mapNode(identity).get, Node("nested", Seq(Leaf("foo"))))
    val nested2 = nextAfter(nested1)
    assertEquals(nested2.map(_.data), Seq("nested", "root"))
    assertEquals(nested2.mapNode(identity).get, Node("nested", Seq(Leaf("bar"))))
  }

  test("find within scope") {
    val root   = Node("root", Seq(Node("nested", Seq(Leaf("foo"))), Leaf("bar")))
    val nested = root.find("nested")
    assertEquals(nested.map(_.data), Seq("nested", "root"))
    assertEquals(nested.mapNode(identity).get, Node("nested", Seq(Leaf("foo"))))
    val foo = nested.mapNode(identity).get.find("foo")
    assertEquals(foo.map(_.data), Seq("foo", "nested"))
    assertEquals(foo.mapNode(identity).get, Leaf("foo"))
    val noBar = nested.mapNode(identity).get.find("bar")
    assertEquals(noBar.valid, false)
    val bar = find(nested, "bar")
    assertEquals(bar.map(_.data), Seq("bar", "root"))
    assertEquals(bar.mapNode(identity).get, Leaf("bar"))
  }

  test("next after sibling") {
    val root  = Node("root", Seq(Leaf("toto"), Leaf("titi"), Node("toto", Seq())))
    val toto1 = root.find("toto")
    assertEquals(toto1.map(_.data), Seq("toto", "root"))
    assertEquals(toto1.mapNode(identity).get, Leaf("toto"))
    val next = nextAfter(toto1)
    assertEquals(next.map(_.data), Seq("titi", "root"))
    val toto2 = find(next, "toto")
    assertEquals(toto2.map(_.data), Seq("toto", "root"))
    assertEquals(toto2.mapNode(identity).get, Node("toto", Seq()))
    val next2 = nextAfter(next)
    assertEquals(next2.map(_.data), Seq("toto", "root"))
    assertEquals(next2.mapNode(identity).get, Node("toto", Seq()))
  }

  test("find equal children") {
    val root  = Node("root", Seq(Leaf("toto"), Leaf("toto")))
    val toto1 = root.find("toto")
    assertEquals(toto1.map(_.data), Seq("toto", "root"))
    assertEquals(toto1.mapNode(identity).get, Leaf("toto"))
    val toto2 = find(nextAfter(toto1), "toto")
    assertEquals(toto2.map(_.data), Seq("toto", "root"))
    assertEquals(toto2.mapNode(identity).get, Leaf("toto"))
    val toto3 = find(nextAfter(toto2), "toto")
    assertEquals(toto3.valid, false)
  }
}
