package io.getquill.mytest

case class Wrap(value: String) extends AnyVal

object Example {
  def main(args: Array[String]): Unit = {
    val ctx = new EncoderContext()
    import ctx._
    val w = new Wrap("stuff")
    SummonAndEncode[Wrap](w) //hellooooooooo
  }
}
