package playground

import java.util.Date
import java.sql.Timestamp
import scala.reflect._
import runtime.{currentMirror}
import runtime.universe._

object Meta {

  val Primitives = Set(
    typeOf[Class[_]], typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
    typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[Boolean], typeOf[Short],
    typeOf[BigDecimal], typeOf[java.lang.Integer], typeOf[java.lang.Long],
    typeOf[java.lang.Double], typeOf[java.lang.Float], typeOf[java.lang.Byte],
    typeOf[java.lang.Boolean], typeOf[Number], typeOf[java.lang.Short], typeOf[Date],
    typeOf[Timestamp], typeOf[scala.Symbol]
  )

  def isPrimitive(tpe: Type) = Primitives exists (_ =:= tpe)

  def optionType(tpe: Type): Type = tpe match {
    case TypeRef(_, _, typeArg :: _) if tpe <:< typeOf[Option[_]] => typeArg
    case _ => throw new RuntimeException(s"${tpe.typeSymbol.name.decoded} is not an option type")
  }

  def listType(tpe: Type): Type = tpe match {
    case TypeRef(_, _, typeArg :: _) if tpe <:< typeOf[Traversable[_]] => typeArg
    case _ => throw new RuntimeException(s"${tpe.typeSymbol.name.decoded} is not a list type")
  }
}