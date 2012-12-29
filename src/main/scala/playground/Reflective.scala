package playground

import scala.reflect.runtime.{currentMirror => cm, universe}
import scala.reflect.runtime.universe._
import scala.reflect._

import java.util.Date
import java.text.SimpleDateFormat

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
      if (values.isComplex(decName)) {
        (bindType(sym._1.typeSignature, values.forPrefix(decName)), sym._2)
      }
	  // Right here we need to inspect the type to make sure it conforms
	  // have symbol, use typeSignature to get type
      else (castPrimativeTypeFromString(sym._1,values(decName).toString), sym._2)
    }
	// Also here need to make sure it conforms to the type
    def optionalValueFor(sym: (Symbol, Int)) = {
		(values.get(sym._1.name.decoded.trim) map {
			v => castPrimativeTypeFromString(sym._1,v.toString)
		}
		, sym._2)
	}
	
	// Here also we need to conform to the right type
    def defaultValueFor(sym: (Symbol, Int)) = (values.get(sym._1.name.decoded.trim).map{
	    x=> castPrimativeTypeFromString(sym._1,x.toString)
	  } getOrElse {
      val ts = im.symbol.typeSignature
      val defarg = ts member newTermName(s"apply$$default$$${sym._2+1}")
      if (defarg != NoSymbol) {
        (im reflectMethod defarg.asMethod)()
      } else
        throw new IllegalArgumentException(s"${sym._1.name.decoded}: ${sym._1.typeSignature.toString}")
    }, sym._2)

    val remainingValues = values -- ctorParams.map(_.name.decoded)
    val toset = (required map valueFor) ::: (options map optionalValueFor) ::: (defaults map defaultValueFor)
	// Initialize the object
    val obj = klazz.reflectConstructor(ctor)(toset.sortBy(_._2).map(_._1):_*)
	// Set any remaining fields here (Good for POJO type DTOS)
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

  // Sets the fields of non constructor args have access to the symbol as well
  // this doesn't look like it can handle non simple fields
  def setFields[S, T : ClassTag](obj: T, values: ValueProvider[S]): T = {
    val im = cm.reflect(obj)
    val ms = im.symbol

    ms.typeSignature.declarations.filter(_.asTerm.isVar) foreach { s =>
	  val f = s.asTerm
      val fm = im.reflectField(f)
      values get f.name.decoded.trim map{ x =>
	    castPrimativeTypeFromString(s,x.toString)
	  } foreach fm.set
    }
    obj
  }
  
  def someString(in:Some[Any]) = in.map{x=>x.toString}
  
  // This could be placed in the ValueProvider trait, but then it will require the passing
  // of the Symbol or Type for every accessor method (get, read, apply, etc...)
  implicit val defaultDateFormat = new SimpleDateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  
  def castPrimativeTypeFromString(sym:Symbol,value:String)(implicit dformatter: SimpleDateFormat):Any = {
    val tpe:Type = sym.typeSignature
    tpe match {
	    case tpe if tpe =:= typeOf[Int] => { value.toInt }
	    case tpe if tpe =:= typeOf[Long] => { value.toLong }
		case tpe if tpe =:= typeOf[Float] => { value.toFloat }
		case tpe if tpe =:= typeOf[Double] => { value.toDouble }
	    case tpe if tpe =:= typeOf[String] => { value }
		case tpe if tpe =:= typeOf[Date] => { dformatter.parse(value) }
	    case _ => { 
			println(s"Error: ${tpe} Not primative type! Value:\n ${value}")
			None 
		}
	  }
  }
}