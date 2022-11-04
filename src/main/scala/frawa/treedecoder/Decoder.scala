package frawa.treedecoder

trait Decoder[Node, Data, T]:
  import Decoder.E
  def decode(node: Node)(using Tree[Node, Data]): Either[E, T]

object Decoder:
  type E = String

  def success[Node, Data, T](value: => T): Decoder[Node, Data, T] = FunDecoder(_ => Right(value))

  def child[Node, Data, T](data: Data, decoder: Decoder[Node, Data, T])(using
      Tree[Node, Data]
  ): Decoder[Node, Data, T] =
    FunDecoder(node =>
      node.findChild(data).map(decoder.decode(_)).getOrElse(Left(s"child '${data}' not found"))
    )

  private class FunDecoder[Node, Data, T](fun: Node => Either[E, T]) extends Decoder[Node, Data, T]:
    def decode(node: Node)(using Tree[Node, Data]): Either[String, T] =
      fun(node)

end Decoder
