package object playground {

  implicit def map2valueProvider(m: Map[String, Any]): ValueProvider[Map[String, Any]] = new MapValueReader(m)

  object PassThroughSeparator extends Separator("", "")
  object DotSeparator extends Separator(".", "")
  object SquareBracketsSeparator extends Separator("[", "]")
  object BracketsSeparator extends Separator("(", ")")
  object ForwardSlashSeparator extends Separator("/", "")
  object BackslashSeparator extends Separator("\\", "")
}
