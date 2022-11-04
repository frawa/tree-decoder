package frawa.treedecoder

import munit.*

class DecoderTest extends FunSuite {
  import Decoder.*
  import TestTree.*

  case class Toto() {}

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

}
