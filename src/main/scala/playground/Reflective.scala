package playground

import scala.reflect.runtime.{currentMirror => cm, universe}
import scala.reflect.runtime.universe._
import scala.reflect._

object Reflective {

  def bind[T](values: ValueProvider[_])(implicit ct: TypeTag[T]): T = bindType(ct.tpe, values).asInstanceOf[T]

  def bindType(tpe: Type, values: ValueProvider[_]): Any = synchronized {
    val klazz = cm.reflectClass(tpe.typeSymbol.asClass)
    val csym = klazz.symbol

    val im = if (csym.isCaseClass) {
      val modul = csym.companionSymbol.asModule
      cm reflect (cm reflectModule modul).instance
    } else null

    val (ctor, ctorParams) = pickConstructor(csym, values.keySet)
    val (defaults, probablyRequired) = ctorParams.zipWithIndex partition (_._1.asTerm.isParamWithDefault)
    val (options, required) = probablyRequired partition (s => s._1.asTerm.typeSignature <:< typeOf[Option[_]])

    def valueFor(sym: (Symbol, Int)) = {
      val decName = sym._1.name.decoded.trim
      val tpe = sym._1.typeSignature
      (readRequired(values, decName, tpe), sym._2)
    }

    def optionalValueFor(sym: (Symbol, Int)) = (readOptional(values, sym._1.name.decoded.trim, Meta.optionType(sym._1.typeSignature)), sym._2)

    def defaultValueFor(sym: (Symbol, Int)) = (readDefault(values, sym._1.name.decoded.trim, sym._1.typeSignature) {
      val ts = im.symbol.typeSignature
      val defarg = ts member newTermName(s"apply$$default$$${sym._2+1}")
      if (defarg != NoSymbol) {
        (im reflectMethod defarg.asMethod)()
      } else
        throw new IllegalArgumentException(s"${sym._1.name.decoded.trim}: ${sym._1.typeSignature.toString}")
    }, sym._2)

    val remainingValues = values -- ctorParams.map(_.name.decoded.trim)
    val toset = (required map valueFor) ::: (options map optionalValueFor) ::: (defaults map defaultValueFor)
    val obj = klazz.reflectConstructor(ctor)(toset.sortBy(_._2).map(_._1):_*)
    setFields(obj, remainingValues)
  }

  private[this] def pickConstructor(clazz: ClassSymbol, argNames: Set[String]): (MethodSymbol, List[Symbol]) = {
    val ctors = clazz.typeSignature.member(nme.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod).sortBy(-_.paramss.sortBy(-_.size).headOption.getOrElse(Nil).size)
    val zipped  = ctors zip (ctors map (ctor => pickConstructorArgs(ctor.paramss, argNames)))
    zipped collectFirst {
      case (m: MethodSymbol, Some(args)) => (m, args)
    } getOrElse (throw new RuntimeException(s"Couldn't find a constructor for ${clazz.name.decoded.trim} and args: [${argNames.mkString(", ")}]"))
  }

  private[this] def pickConstructorArgs(candidates: List[List[Symbol]], argNames: Set[String]): Option[List[Symbol]] = {
    val ctors = candidates.sortBy(-_.size)
    def isRequired(item: Symbol) = {
      val sym = item.asTerm
      !(sym.isParamWithDefault || sym.typeSignature <:< typeOf[Option[_]])
    }
    def matchingRequired(plist: List[Symbol]) = {
      val required = plist filter isRequired
      required.size <= argNames.size && required.forall(s => argNames.contains(s.name.decoded.trim))
    }
    ctors find matchingRequired
  }

  def getFields[T](obj: T)(implicit mf: ClassTag[T]): Seq[(String, Any)] = {
    val im = cm.reflect(obj)
    val ms = im.symbol
    (for {
      decl <- ms.typeSignature.declarations.map(_.asTerm)
      if decl.isVar
      fm = im.reflectField(decl)
    } yield (decl.name.decoded.trim, fm.get)).toSeq
  }

  private[this] def readRequired[S](values: ValueProvider[S], name: String, tpe: Type): Any = {
    if (!Meta.isPrimitive(tpe) && values.isComplex(name)) {
      bindType(tpe, values.forPrefix(name))
    } else values(name)
  }

  private[this] def readOptional[S](values: ValueProvider[S], name: String, tpe: Type): Option[Any] = {
    if (!Meta.isPrimitive(tpe) && values.isComplex(name)) {
      Some(bindType(tpe, values.forPrefix(name)))
    } else values.get(name)
  }

  private[this] def readDefault[S](values: ValueProvider[S], name: String, tpe: Type)(defaultValue: => Any) = {
    if (!Meta.isPrimitive(tpe) && values.isComplex(name)) {
      bindType(tpe, values.forPrefix(name))
    } else values.get(name) getOrElse defaultValue
  }

  def setFields[S, T : ClassTag](obj: T, values: ValueProvider[S]): T = {
    val im = cm.reflect(obj)
    val ms = im.symbol

    ms.typeSignature.declarations.map(_.asTerm).filter(_.isVar) foreach { f =>
      val fm = im.reflectField(f)
      val nm = f.name.decoded.trim
      if (f.typeSignature <:< typeOf[Option[_]])
        fm set readOptional(values, nm, Meta.optionType(f.typeSignature))
      else if (values contains nm)
        fm set readRequired(values, nm, f.typeSignature)
    }
    obj
  }
}