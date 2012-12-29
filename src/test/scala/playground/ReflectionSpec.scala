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

class MutablePersonWithSimpleThing {
  var thing: SimpleThing = _
  var age: Int = _
  
  override def toString() = s"MutablePersonWithSimple(thing: $thing, age: $age)"
  override def equals(obj: Any): Boolean = obj match {
    case o: MutablePersonWithSimpleThing => age == o.age && thing == o.thing
	case _ => false
  }
}

case class SimpleThing(name:String, age:Int)
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
case class OptionThing(name:String, thing:Option[Thing])
case class ListWithStrings(things: List[String])
case class ListWithThings(name: String, things: List[Thing])

class ReflectionSpec extends Specification {

   sequential

  "Reflective access" should {

	// These are not working because java == on Dates just checks that they are the
	// same instance.
    "create a person when all the fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val expected = Person("Timmy", 33, cal.getTime)
      val res = Reflective.bind[Person](Map("name" -> "Timmy", "age" -> "33", "dateOfBirth" -> cal.getTime.toString))
      res.name must_== expected.name
	  res.age must_== expected.age
	  res.dateOfBirth.toString must_== expected.dateOfBirth.toString
    }

    "create a mutable person when all the fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 33, 1, 1, 0, 0, 0)
      val expected = new MutablePerson() //("Timmy", 33, cal.getTime)
      expected.name = "Timmy"
      expected.age = 33
      expected.dateOfBirth = cal.getTime
      val res = Reflective.bind[MutablePerson]( Map("name" -> "Timmy", "age" -> "33", "dateOfBirth" -> cal.getTime.toString))
      res.name must_== expected.name
	  res.age must_== expected.age
	  res.dateOfBirth.toString must_== expected.dateOfBirth.toString
    }
	
	"create a mutable person with a SimpleThing provided" in {
	  val expected = new MutablePersonWithSimpleThing
	  
	  expected.age = 11
	  expected.thing = SimpleThing("Thinger",21)
	  val params = Map("age"->expected.age.toString,
	                   "thing.name"->expected.thing.name.toString,
					   "thing.age"->expected.thing.age.toString)
	  val result = Reflective.bind[MutablePersonWithSimpleThing](params)
	  
	  expected must_== result
	}
	
	"create a mutable person without a SimpleThing" in {
	  val expected = new MutablePersonWithSimpleThing
	  
	  expected.age = 11
	  val params = Map("age"->expected.age.toString)
	  val result = Reflective.bind[MutablePersonWithSimpleThing](params)
	  
	  expected must_== result
	}

    "create a record when all the fields are provided" in {
      val expected = record(225)
      val res = Reflective.bind[Record](Map("id" -> expected.id.toString, "data" -> expected.data.toString, "createdAt" -> expected.createdAt.toString))
      res.id must_== expected.id
	  res.data must_== expected.data
	  res.createdAt.toString must_== expected.createdAt.toString
    }

    "create a record when createdAt is not provided" in {
      val expected = record(225)
      val actual = Reflective.bind[Record](Map("id" -> expected.id.toString, "data" -> expected.data.toString))
      actual.id must_== expected.id
      actual.data must_== expected.data
    }

    "create a thing when only name and age are provided" in {
      val expected = Thing("a thing", 2, None)
      val actual = Reflective.bind[Thing](Map("name" -> expected.name.toString, "age" -> expected.age.toString))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beNone
    }
	
    "create a thing when all fields are provided" in {
      val cal = Calendar.getInstance()
      cal.set(cal.get(Calendar.YEAR) - 2, 1, 1, 0, 0, 0)
      val createdAt = new Date
      val expected = Thing("a thing", 2, None)
      val actual = Reflective.bind[Thing](Map("name" -> expected.name.toString, "age" -> expected.age.toString, "dateOfBirth" -> cal.getTime, "createdAt" -> createdAt.toString))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beSome(cal.getTime)
      actual.createdAt must_== createdAt
    }

    "create another thing when only name and age are provided" in {
      val expected = AnotherThing("another thing", None, 2)
      val actual = Reflective.bind[AnotherThing](Map("name" -> expected.name.toString, "age" -> expected.age.toString))
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.dateOfBirth must beNone
    }

    "create an other record when only id is provided" in {
      val expected = new OtherRecord(303)
      val actual = Reflective.bind[OtherRecord]( Map("id" -> "303"))
      actual.id must_== expected.id
      actual.data must_== expected.data
    }

    "create a person with thing when the necessary data is provided" in {
      val expected = PersonWithThing("tommy", 26, Thing("tommy's thing", 1, None))
      val bound = Reflective.bindType(typeOf[PersonWithThing], Map("name" -> "tommy", "age" -> "26", "thing.name" -> "tommy's thing", "thing.age" -> "1"))
      val actual = bound.asInstanceOf[PersonWithThing]
      actual.name must_== expected.name
      actual.age must_== expected.age
      actual.thing.name must_== expected.thing.name
      actual.thing.age must_== expected.thing.age
    }

  }

  private[this] def record(id: Int) = Record(id, s"A $id record")
}