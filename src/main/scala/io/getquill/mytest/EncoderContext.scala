package io.getquill.mytest

class Encoder[T]:
  def apply(element: T): List[String] = List(element.toString)

class EncoderContext { self =>
  def encode[Cls](cls: Cls) = List(cls.toString)
  implicit inline def anyValEncoder[Cls <: AnyVal]: Encoder[Cls] =
    MappedEncoderMaker[Cls](self)
}