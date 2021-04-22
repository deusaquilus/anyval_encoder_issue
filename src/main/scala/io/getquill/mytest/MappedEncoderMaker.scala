package io.getquill.mytest

import scala.quoted._

object MappedEncoderMaker:
  inline def apply[Mapped <: AnyVal](inline ctx: EncoderContext): Encoder[Mapped] = ${ applyImpl[Mapped]('ctx) }
  def applyImpl[Mapped <: AnyVal: Type](ctx: Expr[EncoderContext])(using Quotes): Expr[Encoder[Mapped]] =
    import quotes.reflect._
    '{ new Encoder[Mapped] { def encode(m: Mapped) = $ctx.encode[Mapped](m) } }

