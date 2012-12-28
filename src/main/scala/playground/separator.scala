package playground

abstract class Separator(val beginning: String, end: String) {

  val hasBeginning = beginning != null && beginning.trim.nonEmpty
  val hasEnd = end != null && end.trim.nonEmpty

  def wrap(part: String, prefix: String = "") = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (hasPrefix) prefix + wrapped(part)
    else part
  }

  def wrapped(part: String) = {
    val sb = new StringBuilder
    if (hasBeginning && !part.startsWith(beginning))
      sb.append(beginning)
    sb.append(part)
    if (hasEnd && !part.endsWith(end))
      sb.append(end)
    sb.toString()
  }

  def stripFirst(key: String) = {
    val endIndex = if (hasEnd) key.indexOf(end) else -1
    def rest = {
      val realEnd = endIndex + end.size
      val hasMore = key.size > (realEnd + 1)
      if (hasMore) key.substring(realEnd) else ""
    }
    if (hasBeginning && key.startsWith(beginning)) {
      if (hasEnd && endIndex > -1) {
         key.substring(beginning.size, endIndex) + rest
      } else key.substring(beginning.size)
    } else if (hasBeginning && hasEnd && endIndex > -1 && endIndex < key.indexOf(beginning)) {
      key.substring(0, endIndex) + rest
    } else key
  }

  def topLevelOnly(key: String, prefix: String = "") = {
    val path = stripPrefix(key, prefix)
    val startIndex = path.indexOf(beginning)
    if (startIndex > -1)
      path.substring(0, startIndex)
    else {
      val endIndex = path.indexOf(end)
      if (hasEnd && endIndex > -1)
        path.substring(0, endIndex)
      else path
    }
  }

  def stripPrefix(path: String, prefix: String) = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (hasPrefix && path.startsWith(prefix)) {
      stripFirst(path.substring(prefix.length))
    }
    else stripFirst(path)
  }
}