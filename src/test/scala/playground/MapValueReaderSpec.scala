package playground

import org.specs2.mutable.Specification

class MapValueReaderSpec extends Specification {
  sequential

  "A MapValueReader with a DotSeparator" should {
    val data = Map("name" -> "Raf", "thing.id" -> 3, "thing.name" -> "a thing", "thingandstrings" -> "member")
    "get a simple key" in {
      val rdr = new MapValueReader(data)
      rdr.get("name") must beSome("Raf")
    }

    "get a composite key" in {
      val rdr = new MapValueReader(data)
      rdr.get("thing.id") must beSome(3)
    }

    "read a simple key" in {
      val rdr = new MapValueReader(data)
      rdr("name") must_== "Raf"
    }

    "read a composite key" in {
      val rdr = new MapValueReader(data)
      rdr("thing.id") must_== 3
    }

    "change root for prefix" in {
      val rdr = new MapValueReader(data).forPrefix("thing")
      rdr("id") must_== 3
      rdr("name") must_== "a thing"
    }

    "values return subset of map for prefix" in {
      val rdr = new MapValueReader(data).forPrefix("thing")
      rdr.values must_== Map("id" -> 3, "name" -> "a thing")
    }

    "only show the top level properties for the keyset" in {
      val rdr = new MapValueReader(data)
      rdr.keySet must_== Set("name", "thing", "thingandstrings")
    }

    "say thing is a complex property" in {
      val rdr = new MapValueReader(data)
      rdr.isComplex("thing") must beTrue
      rdr.isComplex("thingandstrings") must beFalse
      rdr.isComplex("name") must beFalse
    }

  }

  "A MapValueReader with a SquareBracketSeparator" should {
    val data = Map("name" -> "Raf", "thing[id]" -> 3, "thing[name]" -> "a thing", "thingandstrings" -> "member")
    "get a simple key" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr.get("name") must beSome("Raf")
    }

    "get a composite key" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr.get("thing[id]") must beSome(3)
    }

    "read a simple key" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr("name") must_== "Raf"
    }

    "read a composite key" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr("thing[id]") must_== 3
    }

    "change root for prefix" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets).forPrefix("thing")
      rdr("id") must_== 3
      rdr("name") must_== "a thing"
    }

    "values return subset of map for prefix" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets).forPrefix("thing")
      rdr.values must_== Map("id" -> 3, "name" -> "a thing")
    }


    "only show the top level properties for the keyset" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr.keySet must_== Set("name", "thing", "thingandstrings")
    }


    "say thing is a complex property" in {
      val rdr = new MapValueReader(data, separated = by.SquareBrackets)
      rdr.isComplex("thing") must beTrue
      rdr.isComplex("thingandstrings") must beFalse
      rdr.isComplex("name") must beFalse
    }



  }

}