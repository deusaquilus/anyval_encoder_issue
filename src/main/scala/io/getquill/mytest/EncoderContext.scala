package io.getquill.mytest

class Encoder[T] {
  def apply(element: T): List[String] = List(element.toString)
}
case class MappedEncoding[I, O](f: I => O)

class EncoderContext { self =>

  def mappedEncoder[Mapped, Base](mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped] =
    new Encoder[Mapped] {
      override def apply(element: Mapped) = encoder.apply(mapped.f(element))
    }

  implicit inline def anyValEncoder[Cls <: AnyVal]: Encoder[Cls] =
    MappedEncoderMaker[Encoder, Cls](
      new AnyValEncoderContext[Encoder, Cls] {
        override def makeMappedEncoder[Base](mapped: MappedEncoding[Cls, Base], encoder: Encoder[Base]): Encoder[Cls] =
          self; ???
      }
    )

  implicit val stringEncoder: Encoder[String] = 
    new Encoder[String] {
      override def apply(element: String) = List(element)
    }
}