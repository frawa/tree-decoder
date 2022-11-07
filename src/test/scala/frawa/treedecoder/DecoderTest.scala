package frawa.treedecoder

import munit.*

class DecoderTest extends FunSuite {
  import Decoder.*
  import TestTree.*

  case class Toto()                  {}
  case class Titi()                  {}
  case class Gnu(i: Int, toto: Toto) {}
  case class Foo(toto: Toto)         {}

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

  test("firstChild") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = node("root", firstChild(data))
    val value   = decoder.decode(root)
    assertEquals(value, Right("toto"))
  }

  test("next") {
    val root    = Node("root", Seq(Leaf("toto"), Leaf("titi")))
    val decoder = node("root", firstChild(next(data)))
    val value   = decoder.decode(root)
    assertEquals(value, Right("titi"))
  }

  test("map") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = node("toto", success(Toto())).map { toto => Titi() }
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Titi()))
  }

  test("map2") {
    val root    = Node("root", Seq(Leaf("toto")))
    val decoder = map2(node("toto", success(Toto())), success(13)) { (toto, i) => Gnu(i, toto) }
    val gnu     = decoder.decode(root)
    assertEquals(gnu, Right(Gnu(13, Toto())))
  }

  test("flatMap") {
    val root = Node("root", Seq(Leaf("toto"), Leaf("titi")))
    val decoder = node("toto", success(Toto())).flatMap { toto =>
      success(Foo(toto))
    }
    val value = decoder.decode(root)
    assertEquals(value, Right(Foo(Toto())))
  }

  test("seq") {
    val root    = Node("root", Seq(Leaf("toto"), Leaf("titi"), Leaf("toto")))
    val decoder = seq(node("toto", success(Toto())))
    val titi    = decoder.decode(root)
    assertEquals(titi, Right(Seq(Toto(), Toto())))
  }

  test("seq all children") {
    val root =
      Node("root", Seq(Node("foo", Seq(Leaf("toto"), Leaf("titi"), Leaf("toto"))), Leaf("boom")))
    val decoder = node("foo", firstChild(seq(data)))
    val value   = decoder.decode(root)
    assertEquals(value, Right(Seq("toto", "titi", "toto")))
  }

  test("more realistic use case") {
    enum Ast:
      case FunCall(name: String, arguments: Seq[String])

    val parsedTree = Node(
      "parsed",
      Seq(
        Node(
          "chunk",
          Seq(
            Node(
              "FunCall",
              Seq(
                Leaf("chunk"),
                Node(
                  "Name",
                  Seq(
                    Leaf("myFun")
                  )
                ),
                Node(
                  "chunk",
                  Seq(
                    Leaf("junk"),
                    Node(
                      "arguments",
                      Seq(
                        Node("Argument", Seq(Leaf("myOne"))),
                        Node("Argument", Seq(Leaf("myTwo")))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    val decoder =
      node(
        "FunCall",
        map2(
          node("Name", firstChild(data)),
          seq(node("Argument", firstChild(data)))
        ) { case (name, arguments) =>
          Ast.FunCall(name, arguments)
        }
      )

    val ast = decoder.decode(parsedTree)
    assertEquals(ast, Right(Ast.FunCall("myFun", Seq("myOne", "myTwo"))))
  }

}
