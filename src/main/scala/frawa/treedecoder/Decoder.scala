package frawa.treedecoder

trait Decoder[Node, Data, T]:
  import Decoder.E
  import Decoder.Decoded

  def decode(node: Node)(using Tree[Node, Data]): Either[E, T] =
    decode_(Seq(node)).map(_.value)

  def map[S](f: T => S)(using Tree[Node, Data])                          = Decoder.map(this)(f)
  def flatMap[S](f: T => Decoder[Node, Data, S])(using Tree[Node, Data]) = Decoder.flatMap(this)(f)

  protected def decode_(at: Seq[Node])(using Tree[Node, Data]): Either[E, Decoded[Node, T]]

object Decoder:
  type E = String

  case class Decoded[Node, T](value: T, at: Seq[Node]) {
    def map[S](f: T => S): Decoded[Node, S]     = this.copy(value = f(value))
    def flatMap[S](f: T => S): Decoded[Node, S] = this.copy(value = f(value))
  }

  def success[Node, Data, T](value: => T)(using Tree[Node, Data]): Decoder[Node, Data, T] =
    FunDecoder(at => Right(Decoded(value, at)))

  def data[Node, Data]: Decoder[Node, Data, Data] =
    DataDecoder()

  def node[Node, Data, T](data: Data, decoder: Decoder[Node, Data, T])(using
      Tree[Node, Data]
  ): Decoder[Node, Data, T] =
    FunDecoder(at =>
      // TODO replace Seq by NonEmptySeq?
      val found = TreeFinder.find(at, data)
      if found.nonEmpty then
        decoder
          .decode_(Seq(found.head))
          .map { d =>
            Decoded(d.value, d.at ++ found.drop(1))
          }
      else Left(s"node '${data}' not found under '${at.map(_.data).reverse.mkString(".")}'")
    )

  def map[Node, Data, T, S](decoder: Decoder[Node, Data, T])(f: T => S)(using
      Tree[Node, Data]
  ): Decoder[Node, Data, S] =
    FunDecoder(at => decoder.decode_(at).map(_.map(f)))

  def flatMap[Node, Data, T, S](decoder: Decoder[Node, Data, T])(f: T => Decoder[Node, Data, S])(
      using Tree[Node, Data]
  ): Decoder[Node, Data, S] =
    FunDecoder(at =>
      decoder
        .decode_(at)
        .map(decoded => decoded.map(f))
        .flatMap(d => d.value.decode_(d.at))
    )

  def seq[Node, Data, T](decoder: Decoder[Node, Data, T])(using
      Tree[Node, Data]
  ): Decoder[Node, Data, Seq[T]] =
    FunDecoder { at =>
      val start = decoder.decode_(at)
      Right(
        Iterator
          .iterate(start)(d =>
            d.flatMap(d =>
              val next = TreeFinder.nextAfter(d.at)
              decoder.decode_(next)
            )
          )
          .takeWhile(_.isRight)
          .flatMap(_.toOption)
          .foldLeft(Decoded(Seq[T](), at))((acc, d) => d.copy(value = acc.value :+ d.value))
      )
    }

  private class FunDecoder[Node, Data, T](fun: Seq[Node] => Either[E, Decoded[Node, T]])
      extends Decoder[Node, Data, T]:
    def decode_(at: Seq[Node])(using Tree[Node, Data]): Either[String, Decoded[Node, T]] =
      fun(at)

  private class DataDecoder[Node, Data] extends Decoder[Node, Data, Data]:
    def decode_(at: Seq[Node])(using Tree[Node, Data]): Either[String, Decoded[Node, Data]] =
      Right(Decoded(at.head.data, at))

end Decoder
