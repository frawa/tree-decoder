package frawa.treedecoder

import munit.*

class DecoderTest extends FunSuite {
  import Decoder.*
  import TestTree.*

  case class Toto()          {}
  case class Titi()          {}
  case class Foo(toto: Toto) {}

  test("not found") {
    val root    = Node("root")
    val decoder = node("toto", success(Toto()))
    val toto    = decoder.decode(root)
    assertEquals(toto, Left("node 'toto' not found under 'root'"))
  }

  test("success") {
    val root    = Node("root")
    val decoder = success[TestTree, String, Toto](Toto())
    val toto    = decoder.decode(root)
    assertEquals(toto, Right(Toto()))
  }

  test("node") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = node("toto", success(Toto()))
    val toto    = decoder.decode(root)
    assertEquals(toto, Right(Toto()))
  }

  test("data") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = node("toto", data)
    val value   = decoder.decode(root)
    assertEquals(value, Right("toto"))
  }

  test("map") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = node("toto", success(Toto())).map { toto => Titi() }
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Titi()))
  }

  test("flatMap") {
    val root = Node("root", Seq(Leaf("toto"), Leaf("titi")))
    val decoder = node("toto", success(Toto())).flatMap { toto =>
      success(Foo(toto))
    }
    val value = decoder.decode(root)
    assertEquals(value, Right(Foo(Toto())))
  }

  test("seq".ignore) {
    val root    = Node("root", Seq(Leaf("toto"), Leaf("titi"), Leaf("toto")))
    val decoder = seq(node("toto", success(Toto())))
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Seq(Toto(), Toto())))
  }

}
