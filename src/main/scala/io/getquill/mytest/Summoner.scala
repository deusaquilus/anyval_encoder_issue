package io.getquill.mytest

import scala.quoted._

object SummonAndEncode {
  inline def apply[Cls <: AnyVal](cls: Cls): Unit = ${ applyImpl[Cls]('cls) }
  def applyImpl[Cls <: AnyVal: Type](cls: Expr[Cls])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    Expr.summon[Encoder[Cls]] match
      case Some(value) => println(s"ENCODER FOUND")
      case None => println("ENCODER NOT FOUND")
    '{ () }
  }
}