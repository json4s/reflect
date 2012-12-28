package playground

import scala.reflect.runtime.{currentMirror => cm, universe}
import scala.reflect.runtime.universe._
import scala.reflect._

object Reflective {


//  private[this] val reflective = cm.reflect(cm.reflectModule(cm.moduleSymbol(getClass)).instance)
//  private[this] def bindType(tpe: Type, values: ValueProvider[_]) {
//    val meth = reflective.symbol.typeSignature.member(newTermName("bind")).asMethod
//    meth.typeSignature.substituteTypes(meth.typeParams, List(tpe))
//    reflective.reflectMethod(meth).apply(values)
//  }

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
      if (values.isComplex(decName)) {
//        ClassTag(sym._1.typeSignature.typeSymbol.asClass)
//
        (bindType(sym._1.typeSignature, values.forPrefix(decName)), sym._2)
      }
      else (values(decName), sym._2)
    }

    def optionalValueFor(sym: (Symbol, Int)) = (values.get(sym._1.name.decoded.trim), sym._2)

    def defaultValueFor(sym: (Symbol, Int)) = (values.get(sym._1.name.decoded.trim) getOrElse {
      val ts = im.symbol.typeSignature
      val defarg = ts member newTermName(s"apply$$default$$${sym._2+1}")
      if (defarg != NoSymbol) {
        (im reflectMethod defarg.asMethod)()
      } else
        throw new IllegalArgumentException(s"${sym._1.name.decoded}: ${sym._1.typeSignature.toString}")
    }, sym._2)

    val remainingValues = values -- ctorParams.map(_.name.decoded)
    val toset = (required map valueFor) ::: (options map optionalValueFor) ::: (defaults map defaultValueFor)
    val obj = klazz.reflectConstructor(ctor)(toset.sortBy(_._2).map(_._1):_*)
    setFields(obj, remainingValues)
  }


  def bind[T](values: ValueProvider[_])(implicit ct: ClassTag[T]): T = synchronized {
    val klazz = cm.reflectClass(cm.classSymbol(ct.runtimeClass))
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
      if (values.isComplex(decName)) {
//        ClassTag(sym._1.typeSignature.typeSymbol.asClass.)
        (bindType(sym._1.typeSignature, values.forPrefix(decName)), sym._2)
      }
      else (values(decName), sym._2)
    }

    def optionalValueFor(sym: (Symbol, Int)) = (values.get(sym._1.name.decoded.trim), sym._2)

    def defaultValueFor(sym: (Symbol, Int)) = (values.get(sym._1.name.decoded.trim) getOrElse {
      val ts = im.symbol.typeSignature
      val defarg = ts member newTermName(s"apply$$default$$${sym._2+1}")
      if (defarg != NoSymbol) {
        (im reflectMethod defarg.asMethod)()
      } else
        throw new IllegalArgumentException(s"${sym._1.name.decoded}: ${sym._1.typeSignature.toString}")
    }, sym._2)

    val remainingValues = values -- ctorParams.map(_.name.decoded)
    val toset = (required map valueFor) ::: (options map optionalValueFor) ::: (defaults map defaultValueFor)
    val obj = klazz.reflectConstructor(ctor)(toset.sortBy(_._2).map(_._1):_*).asInstanceOf[T]
    setFields(obj, remainingValues)
  }

  private[this] def pickConstructor(clazz: ClassSymbol, argNames: Set[String]): (MethodSymbol, List[Symbol]) = {
    val ctors = clazz.typeSignature.member(nme.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod).sortBy(-_.paramss.sortBy(-_.size).headOption.getOrElse(Nil).size)
    val zipped  = ctors zip (ctors map (ctor => pickConstructorArgs(ctor.paramss, argNames)))
    zipped collectFirst {
      case (m: MethodSymbol, Some(args)) => (m, args)
    } getOrElse (throw new RuntimeException(s"Couldn't find a constructor for ${clazz.name.decoded} and args: [${argNames.mkString(", ")}]"))
  }

  private[this] def pickConstructorArgs(candidates: List[List[Symbol]], argNames: Set[String]): Option[List[Symbol]] = {
    val ctors = candidates.sortBy(-_.size)
    def isRequired(item: Symbol) = {
      val sym = item.asTerm
      !(sym.isParamWithDefault || sym.typeSignature <:< typeOf[Option[_]])
    }
    def matchingRequired(plist: List[Symbol]) = {
      val required = plist filter isRequired
      required.size <= argNames.size && required.forall(s => argNames.contains(s.name.decoded))
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

  def setFields[S, T : ClassTag](obj: T, values: ValueProvider[S]): T = {
    val im = cm.reflect(obj)
    val ms = im.symbol

    ms.typeSignature.declarations.map(_.asTerm).filter(_.isVar) foreach { f =>
      val fm = im.reflectField(f)
      values get f.name.decoded.trim foreach fm.set
    }
    obj
  }
}