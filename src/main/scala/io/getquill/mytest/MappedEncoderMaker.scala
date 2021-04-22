package io.getquill.mytest

import scala.quoted._

object MappedEncoderMaker:
  inline def apply[T <: AnyVal](inline ctx: EncoderContext): Encoder[T] = ${ applyImpl[T]('ctx) }
  def applyImpl[T <: AnyVal: Type](ctx: Expr[EncoderContext])(using Quotes): Expr[Encoder[T]] =
    import quotes.reflect._
    println(s"===== Creating Instance for: ${Printer.TypeReprShortCode.show(TypeRepr.of[T])}")
    '{ new Encoder[T] { def encode(m: T) = $ctx.encode[T](m) } }

