package playground

import util.control.Exception._

trait ValueProvider[S]  {
  def prefix: String
  def separator: Separator
  protected def data: S
  def read(key: String): Either[Throwable, Option[Any]]
  def get(key: String): Option[Any] = read(key).fold(_ => None, identity)
  def apply(key: String): Any = read(key).fold(throw _, _ getOrElse (throw new RuntimeException(s"No entry found for $key")))
  def forPrefix(key: String): ValueProvider[S]
  def values: S
  def keySet: Set[String]
  def --(keys: Iterable[String]): ValueProvider[S]
  def isComplex(key: String): Boolean
}

class MapValueReader(protected val data: Map[String, Any], val prefix: String = "", val separator: Separator = DotSeparator) extends ValueProvider[Map[String, Any]] {

  override def toString = data.toString

  def read(key: String): Either[Throwable, Option[Any]] = allCatch either { data get separator.wrap(key, prefix) }

  def forPrefix(key: String): ValueProvider[Map[String, Any]] = new MapValueReader(data, separator.wrap(key), separator)

  lazy val values: Map[String, Any] = stripPrefix(data)

  def keySet: Set[String] = values.keySet map (separator.topLevelOnly(_, prefix))

  def --(keys: Iterable[String]) = new MapValueReader(data -- keys.map(separator.wrap(_, prefix)), prefix, separator)

  def isComplex(key: String) = {
    val pref = separator.wrap(key, prefix)
    if (pref != null && pref.trim.nonEmpty) {
      data exists {
        case (k, _)  =>
          separator.stripPrefix(k, prefix).contains(separator.beginning) && k.startsWith(pref + separator.beginning)
      }
    } else false
  }

  private[this] def stripPrefix(d: Map[String, Any]): Map[String, Any] = {
    if (prefix != null && prefix.trim.nonEmpty) {
      d collect {
        case (k, v) if k startsWith (prefix + separator.beginning) => separator.stripPrefix(k, prefix) -> v
      }
    } else d
  }
}



