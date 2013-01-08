package playground

import util.control.Exception._

trait ValueProvider[S]  {
  def prefix: String
  def separated: Separator
  protected def data: S
  def read(key: String): Either[Throwable, Option[Any]]
  def get(key: String): Option[Any] = read(key).fold(_ => None, identity)
  def apply(key: String): Any = read(key).fold(throw _, _ getOrElse (throw new RuntimeException(s"No entry found for $key")))
  def forPrefix(key: String): ValueProvider[S]
  def values: S
  def keySet: Set[String]
  def --(keys: Iterable[String]): ValueProvider[S]
  def isComplex(key: String): Boolean
  def contains(key: String): Boolean
  def getUniquePrefixes: Set[String]
}

object MapValueReader {
  def apply(data: Map[String, Any], separated: Separator = by.Dots) = new MapValueReader(data, separated = separated)
}

class MapValueReader(protected val data: Map[String, Any], val prefix: String = "", val separated: Separator = by.Dots) extends ValueProvider[Map[String, Any]] {
  
  def getUniquePrefixes: Set[String] = data.collect{ 
    case (key,_) if key.startsWith(prefix) => separated.topLevelOnly(key,prefix)
  }.toSet
  
  def read(key: String): Either[Throwable, Option[Any]] = allCatch either { data get separated.wrap(key, prefix) }

  def forPrefix(key: String): ValueProvider[Map[String, Any]] = new MapValueReader(data, separated.wrap(key, prefix), separated)

  lazy val values: Map[String, Any] = stripPrefix(data)

  def keySet: Set[String] = values.keySet map (separated.topLevelOnly(_, prefix))

  def --(keys: Iterable[String]) = new MapValueReader(data -- keys.map(separated.wrap(_, prefix)), prefix, separated)

  def isComplex(key: String) = {
    val pref = separated.wrap(key, prefix)
    if (pref != null && pref.trim.nonEmpty) {
      data exists {
        case (k, _)  =>
          separated.stripPrefix(k, prefix).contains(separated.beginning) && k.startsWith(pref + separated.beginning)
      }
    } else false
  }


  def contains(key: String): Boolean = (data contains separated.wrap(key, prefix)) || isComplex(key)

  private[this] def stripPrefix(d: Map[String, Any]): Map[String, Any] = {
    if (prefix != null && prefix.trim.nonEmpty) {
      d collect {
        case (k, v) if k startsWith (prefix + separated.beginning) => separated.stripPrefix(k, prefix) -> v
      }
    } else d
  }
}



