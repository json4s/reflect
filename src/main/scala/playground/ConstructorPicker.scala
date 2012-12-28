package playground

import scala.reflect.runtime.{currentMirror => cm, universe}
import scala.reflect.runtime.universe._
import scala.reflect._

class ConstructorPicker {
  def apply(p: Class[_]) = ???
}