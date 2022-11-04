package frawa.treedecoder

import munit.*

class DecoderTest extends FunSuite {
  import Decoder.*
  import TestTree.*

  case class Toto() {}
  case class Titi() {}

  test("not found") {
    val root    = Node("root")
    val decoder = child("toto", success(Toto()))
    val toto    = decoder.decode(root)
    assertEquals(toto, Left("child 'toto' not found"))
  }

  test("success") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = child("toto", success(Toto()))
    val toto    = decoder.decode(root)
    assertEquals(toto, Right(Toto()))
  }

  test("map") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = map(child("toto", success(Toto()))) { toto => Titi() }
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Titi()))
  }

  test("seq") {
    val root    = Node("root", Seq(Leaf("toto"), Leaf("titi"), Leaf("toto")))
    val decoder = seq(child("toto", success(Toto())))
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Seq(Toto(), Toto())))
  }

}
