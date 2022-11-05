package frawa.treedecoder

trait Decoder[Node, Data, T]:
  import Decoder.E
  import Decoder.Decoded

  def decode(node: Node)(using Tree[Node, Data]): Either[E, T] =
    decode_(Seq(node)).map(_.value)

  protected def decode_(path: Seq[Node])(using Tree[Node, Data]): Either[E, Decoded[Node, T]]

object Decoder:
  type E = String

  case class Decoded[Node, T](value: T, path: Seq[Node]) {
    def map[S](f: T => S): Decoded[Node, S] = this.copy(value = f(value))
  }

  def success[Node, Data, T](value: => T): Decoder[Node, Data, T] =
    FunDecoder(path => Right(Decoded(value, path)))

  def data[Node, Data]: Decoder[Node, Data, Data] =
    DataDecoder()

  def child[Node, Data, T](data: Data, decoder: Decoder[Node, Data, T])(using
      Tree[Node, Data]
  ): Decoder[Node, Data, T] =
    FunDecoder(path =>
      if path.size == 1 then
        val found = path.head.findChild(data)
        if found.nonEmpty then decoder.decode_(found)
        else Left(s"child '${data}' not found")
      else // TODO dedup
        val found = TreeFinder.findNext(path, data)
        if found.nonEmpty then decoder.decode_(found)
        else Left(s"child '${data}' not found")
    )

  def map[Node, Data, T, S](decoder: Decoder[Node, Data, T])(f: T => S)(using
      Tree[Node, Data]
  ): Decoder[Node, Data, S] =
    FunDecoder(path => decoder.decode_(path).map(_.map(f)))

  def seq[Node, Data, T](decoder: Decoder[Node, Data, T])(using
      Tree[Node, Data]
  ): Decoder[Node, Data, Seq[T]] =
    FunDecoder { path =>
      val start = decoder.decode_(path)
      start.map { _ =>
        val values = Iterator
          .iterate(start)(decoded => decoded.flatMap(decoded => decoder.decode_(decoded.path)))
          .takeWhile(decoded => decoded.isRight)
          .flatMap(decoded => decoded.map(decoded => Seq(decoded.value)).getOrElse(Seq()))
          .toSeq
        Decoded(values, path)
      }
    }

  private class FunDecoder[Node, Data, T](fun: Seq[Node] => Either[E, Decoded[Node, T]])
      extends Decoder[Node, Data, T]:
    def decode_(path: Seq[Node])(using Tree[Node, Data]): Either[String, Decoded[Node, T]] =
      fun(path)

  private class DataDecoder[Node, Data] extends Decoder[Node, Data, Data]:
    def decode_(path: Seq[Node])(using Tree[Node, Data]): Either[String, Decoded[Node, Data]] =
      Right(Decoded(path.head.data(), path))

end Decoder
