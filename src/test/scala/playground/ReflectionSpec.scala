package playground

import org.specs2.mutable.Specification
import java.util.{Calendar, Date}
import scala.reflect.runtime.universe._

class MutablePerson {
  var name: String = _
  var age: Int = _
  var dateOfBirth: Date = _
  override def toString() = "MutablePerson(name: %s, age: %d, dateOfBirth: %s)".format(name, age, dateOfBirth)

  override def equals(obj: Any): Boolean = obj match {
    case o: MutablePerson => name == o.name && age == o.age && dateOfBirth == o.dateOfBirth
    case _ => false
  }
}

class MutablePersonWithThing {
  var name: String = _
  var age: Int = _
  var dateOfBirth: Date = _
  var thing: Thing = _

  override def toString() = "MutablePersonWithThing(name: %s, age: %d, dateOfBirth: %s, thing: %s)".format(name, age, dateOfBirth, thing)

  override def equals(obj: Any): Boolean = obj match {
    case o: MutablePerson => name == o.name && age == o.age && dateOfBirth == o.dateOfBirth
    case _ => false
  }
}

class MutableTestWithDefault {
  var name: String = _
  var age: Int = 2
  override def toString() = "MutableTestWithDefault(name: %s, age: %d)".format(name, age)
}

class MutableWithOption {
  var name: String = _
  var age: Option[Int] = None
  override def toString() = "MutableWithOption(name: %s, age: %s)".format(name, age)
}

class MutableWithOptionThing {
  var name: String = _
  var thing: Option[Thing] = None
  override def toString() = "MutableWithOptionThing(name: %s, thing: %s)".format(name, thing)
}

case class Person(name: String, age: Int, dateOfBirth: Date)
case class Thing(name: String, age: Int, dateOfBirth: Option[Date], createdAt: Date = new Date)
case class AnotherThing(name: String, dateOfBirth: Option[Date], age: Int, createdAt: Date = new Date)
case class Record(id: Int, data: String, createdAt: Date = new Date)
case class OtherRecord(id: Int, data: String, createdAt: Date = new Date) {
  def this(id: Int) = this(id, "")
  def this() = this(0, "")
}
case class PersonWithThing(name: String, age: Int, thing: Thing)
case class OptionTester(name:String, age:Option[Int])
case class OptionWithDefaultTester(name: String, age: Option[Int] = Some(3))
case class OptionThing(name:String, thing:Option[Thing])
case class ListWithStrings(things: List[String])
case class ListWithThings(name: String, things: List[Thing])

class ReflectionSpec extends Specification {

  sequential
  "Reflective access" should {

    "bind a mutable option with thing" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val thing = Thing("antenna", 1, Some(cal.getTime))
      val expected = new MutableWithOptionThing
      expected.name = "radio"
      expected.thing = Some(thing)
      val data = Map(
        "name" -> expected.name,
        "thing.name" -> thing.name,
        "thing.age" -> thing.age,
        "thing.dateOfBirth" -> thing.dateOfBirth.get,
        "thing.createdAt" -> thing.createdAt)
      val actual = Reflective.bind[MutableWithOptionThing](data)
      actual.name must_== expected.name
      actual.thing must_== expected.thing
    }

    "use the value provided for an option on a mutable field" in {
      val expected = new MutableWithOption
      expected.name = "mutaphone"
      expected.age = Some(5)
      val actual = Reflective.bind[MutableWithOption](Map("name" -> expected.name, "age" -> expected.age.get))
      actual.name must_== expected.name
      actual.age must_== expected.age
    }

    "use the default value for a mutable field" in {
      val expected = new MutableTestWithDefault
      expected.name = "mutaphone"
      val actual = Reflective.bind[MutableTestWithDefault](Map("name" -> expected.name))
      actual.name must_== expected.name
      actual.age must_== expected.age
    }

    "set a value for a mutable field with a default value" in {
      val expected = new MutableTestWithDefault
      expected.name = "mutaphone"
      expected.age = 5
      val actual = Reflective.bind[MutableTestWithDefault](Map("name" -> expected.name, "age" -> expected.age))
      actual.name must_== expected.name
      actual.age must_== expected.age
    }

    "use the default value for an option when no value is provided" in {
      val expected = OptionWithDefaultTester("cell")
      val actual = Reflective.bind[OptionWithDefaultTester](Map("name" -> expected.name))
      actual must_== expected
    }

    "create an option thing when thing is provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val thing = Thing("antenna", 1, Some(cal.getTime))
      val expected = OptionThing("radio", Some(thing))
      val data = Map(
        "name" -> expected.name,
        "thing.name" -> thing.name,
        "thing.age" -> thing.age,
        "thing.dateOfBirth" -> thing.dateOfBirth.get,
        "thing.createdAt" -> thing.createdAt)
      val actual = Reflective.bind[OptionThing](data)
      actual must_== expected
    }

    "create an option thing when thing is NOT provided" in {
      val expected = OptionThing("phone", None)
      val actual = Reflective.bind[OptionThing](Map("name" -> expected.name))
      actual must_== expected
    }

    "create a person when all the fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val expected = Person("Timmy", 33, cal.getTime)
      val res = Reflective.bind[Person](Map("name" -> "Timmy", "age" -> 33, "dateOfBirth" -> cal.getTime))
      res must_== expected
    }

    "create a mutable person when all the fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val expected = new MutablePerson() //("Timmy", 33, cal.getTime)
      expected.name = "Timmy"
      expected.age = 33
      expected.dateOfBirth = cal.getTime
      val res = Reflective.bindType(typeOf[MutablePerson], Map("name" -> "Timmy", "age" -> 33, "dateOfBirth" -> cal.getTime))
      res must_== expected
    }

    "create a record when all the fields are provided" in {
      val expected = record(225)
      val res = Reflective.bind[Record](Map("id" -> expected.id, "data" -> expected.data, "createdAt" -> expected.createdAt))
      res must_== expected
    }

    "create a record when createdAt is not provided" in {
      val expected = record(225)
      val actual = Reflective.bind[Record](Map("id" -> expected.id, "data" -> expected.data))
      actual.id must_== expected.id
      actual.data must_== expected.data
    }

    "create a thing when only name and age are provided" in {
      val expected = Thing("a thing", 2, None)
      val actual = Reflective.bind[Thing](Map("name" -> expected.name, "age" -> expected.age))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beNone
    }

    "create a thing when all fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 2, 1, 1, 0, 0, 0)
      val createdAt = new Date
      val expected = Thing("a thing", 2, None)
      val actual = Reflective.bind[Thing](Map("name" -> expected.name, "age" -> expected.age, "dateOfBirth" -> cal.getTime, "createdAt" -> createdAt))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beSome(cal.getTime)
      actual.createdAt must_== createdAt
    }

    "create another thing when only name and age are provided" in {
      val expected = AnotherThing("another thing", None, 2)
      val actual = Reflective.bind[AnotherThing](Map("name" -> expected.name, "age" -> expected.age))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beNone
    }

    "create an other record when only id is provided" in {
      val expected = new OtherRecord(303)
      val actual = Reflective.bind[OtherRecord]( Map("id" -> 303))
      actual.id must_== expected.id
      actual.data must_== expected.data
    }

    "create a person with thing when the necessary data is provided" in {
      val expected = PersonWithThing("tommy", 26, Thing("tommy's thing", 1, None))
      val bound = Reflective.bindType(typeOf[PersonWithThing], Map("name" -> "tommy", "age" -> 26, "thing.name" -> "tommy's thing", "thing.age" -> 1))
      val actual = bound.asInstanceOf[PersonWithThing]
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.thing.name must_== expected.thing.name
      actual.thing.age must_== expected.thing.age
    }

    "create a person with thing when the necessary data is provided as an object" in {
      val expected = PersonWithThing("tommy", 26, Thing("tommy's thing", 1, None))
      val thing = Thing("tommy's thing", 1, None)
      val bound = Reflective.bindType(typeOf[PersonWithThing], Map("name" -> "tommy", "age" -> 26, "thing" -> thing))
      val actual = bound.asInstanceOf[PersonWithThing]
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.thing.name must_== expected.thing.name
      actual.thing.age must_== expected.thing.age
    }


    "create a mutable person with thing when the necessary data is provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val expected = new MutablePersonWithThing
      expected.name = "tommy"
      expected.age = 26
      expected.dateOfBirth = cal.getTime
      expected.thing = Thing("tommy's thing", 1, None)
      val data = Map("name" -> "tommy", "age" -> 26, "dateOfBirth" -> cal.getTime, "thing.name" -> "tommy's thing", "thing.age" -> 1)
      val bound = Reflective.bindType(typeOf[MutablePersonWithThing], data)
      val actual = bound.asInstanceOf[MutablePersonWithThing]
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must_== expected.dateOfBirth
      actual.thing.name must_== expected.thing.name
      actual.thing.age must_== expected.thing.age
    }

    "create an option tester when optional data is available" in {
      val expected = OptionTester("halo", Some(3))
      val actual = Reflective.bind[OptionTester](Map("name" -> expected.name, "age" -> 3))
      actual must_== expected
    }

  }

  private[this] def record(id: Int) = Record(id, s"A $id record")
}