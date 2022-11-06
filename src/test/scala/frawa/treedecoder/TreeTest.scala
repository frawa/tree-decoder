package frawa.treedecoder

import munit.*

class TreeTest extends FunSuite {
  import TestTree.*

  test("empty") {
    val emptyRoot = Node("root")
    assertEquals(emptyRoot.data, "root");
    assertEquals(emptyRoot.find("root"), Seq(emptyRoot));
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
    assertEquals(nested1.head, Node("nested", Seq(Leaf("foo"))))
    val nested2 = TreeFinder.nextAfter(nested1)
    assertEquals(nested2.map(_.data), Seq("nested", "root"))
    assertEquals(nested2.head, Node("nested", Seq(Leaf("bar"))))
  }

  test("find within scope") {
    val root   = Node("root", Seq(Node("nested", Seq(Leaf("foo"))), Leaf("bar")))
    val nested = root.find("nested")
    assertEquals(nested.map(_.data), Seq("nested", "root"))
    assertEquals(nested.head, Node("nested", Seq(Leaf("foo"))))
    val foo = nested.head.find("foo")
    assertEquals(foo.map(_.data), Seq("foo", "nested"))
    assertEquals(foo.head, Leaf("foo"))
    val noBar = nested.head.find("bar")
    assertEquals(noBar, Seq())
    val bar = TreeFinder.find(nested, "bar")
    assertEquals(bar.map(_.data), Seq("bar", "root"))
    assertEquals(bar.head, Leaf("bar"))
  }

  test("next after sibling") {
    val root  = Node("root", Seq(Leaf("toto"), Leaf("titi"), Node("toto", Seq())))
    val toto1 = root.find("toto")
    assertEquals(toto1.map(_.data), Seq("toto", "root"))
    assertEquals(toto1.head, Leaf("toto"))
    val next = TreeFinder.nextAfter(toto1)
    assertEquals(next.map(_.data), Seq("titi", "root"))
    val toto2 = TreeFinder.find(next, "toto")
    assertEquals(toto2.map(_.data), Seq("toto", "root"))
    assertEquals(toto2.head, Node("toto", Seq()))
    val next2 = TreeFinder.nextAfter(next)
    assertEquals(next2.map(_.data), Seq("toto", "root"))
    assertEquals(next2.head, Node("toto", Seq()))
  }

  test("find equal children") {
    val root  = Node("root", Seq(Leaf("toto"), Leaf("toto")))
    val toto1 = root.find("toto")
    assertEquals(toto1.map(_.data), Seq("toto", "root"))
    assertEquals(toto1.head, Leaf("toto"))
    val toto2 = TreeFinder.find(toto1, "toto")
    assertEquals(toto2.map(_.data), Seq("toto", "root"))
    assertEquals(toto2.head, Leaf("toto"))
    val toto3 = TreeFinder.find(toto2, "toto")
    assertEquals(toto3, Seq())
  }
}
