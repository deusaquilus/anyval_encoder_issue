package io.getquill.mytest

import scala.quoted._

trait AnyValEncoderContext[Encoder[_], Mapped] {
  def makeMappedEncoder[Base](mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]
}

object MappedEncoderMaker:
  inline def apply[Encoder[_], Mapped <: AnyVal](inline ctx: AnyValEncoderContext[Encoder, Mapped]): Encoder[Mapped] = ${ applyImpl[Encoder, Mapped]('ctx) }
  def applyImpl[Encoder[_]: Type, Mapped <: AnyVal: Type](ctx: Expr[AnyValEncoderContext[Encoder, Mapped]])(using qctx: Quotes): Expr[Encoder[Mapped]] =
    import qctx.reflect._
    val tpe = TypeRepr.of[Mapped]
    val firstParam =
      tpe.typeSymbol.primaryConstructor.paramSymss match
        case List(List(first)) => 
          first
        case _ => 
          report.throwError(s"not matched: ${tpe.dealias.show}")
    val firstParamField = tpe.typeSymbol.memberField(firstParam.name)
    val firstParamType = tpe.memberType(firstParamField)

    // Try to summon an encoder from the first param type
    firstParamType.asType match
      case '[tt] =>
        Expr.summon[Encoder[tt]] match
          case Some(enc) => 
            val mappedEncoding = '{ MappedEncoding((v:Mapped) => ${ Select('v.asTerm, firstParamField).asExprOf[tt] }) }
            val out = '{ $ctx.makeMappedEncoder[tt]($mappedEncoding, $enc) }
            println(s"========== RETURNING Encoder ${tpe.show} => ${firstParamType.show} Consisting of: ${out.show} =========")
            out
          case None => 
            report.throwError(
              s"Cannot find a regular encoder for the AnyVal type ${tpe.show} or a mapped-encoder for it's base type: ${firstParamType.show}"
            )
end MappedEncoderMaker
