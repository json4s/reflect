package playground

import org.specs2.mutable.Specification

class SeparatorSpec extends Specification {
  val dots = DotSeparator
  val squareBrackets = SquareBracketsSeparator
  "A DotSeparator" should {
    "strip the separator from a key" in {
      dots.stripFirst(".hello") must_== "hello"
      dots.stripFirst(".hello.there") must_== "hello.there"
    }
    "wrap a key for prefix" in {
      dots.wrap("name", "person") must_== "person.name"
      dots.wrap("name", "") must_== "name"
    }
    "wrap a key" in {
      dots.wrapped("name") must_== ".name"
      dots.wrapped(".name") must_== ".name"
    }
    "strip the prefix from a path" in {
      dots.stripPrefix("person.address.street", "person") must_== "address.street"
      dots.stripPrefix("address.street", "person") must_== "address.street"
      dots.stripPrefix(".address.street", "person") must_== "address.street"
      dots.stripPrefix("some.person.address.street", "person") must_== "some.person.address.street"
    }
    "get the toplevel property only for a given prefix" in {
      dots.topLevelOnly("person.address.street", "person") must_== "address"
      dots.topLevelOnly("address", "person") must_== "address"
      dots.topLevelOnly("address.street", "person") must_== "address"
      dots.topLevelOnly(".address.street", "person") must_== "address"
      dots.topLevelOnly("some.person.address.street", "person") must_== "some"
    }
  }
  "A SquareBracketSeparator" should {
    "strip the separator from a key" in {
      squareBrackets.stripFirst("[hello]") must_== "hello"
      squareBrackets.stripFirst("[hello][there]") must_== "hello[there]"
    }
    "wrap a key for prefix" in {
      squareBrackets.wrap("name", "person") must_== "person[name]"
      squareBrackets.wrap("name", "") must_== "name"
    }
    "wrap a key" in {
      squareBrackets.wrapped("name") must_== "[name]"
      squareBrackets.wrapped("[name]") must_== "[name]"
    }
    "strip the prefix from a path" in {
      squareBrackets.stripPrefix("person[address][street]", "person") must_== "address[street]"
      squareBrackets.stripPrefix("address[street]", "person") must_== "address[street]"
      squareBrackets.stripPrefix("[address][street]", "person") must_== "address[street]"
      squareBrackets.stripPrefix("some[person][address][street]", "person") must_== "some[person][address][street]"
      squareBrackets.stripPrefix("some][person][address][street]", "person") must_== "some[person][address][street]"
    }
    "get the toplevel property only for a given prefix" in {
      squareBrackets.topLevelOnly("person[address][street]", "person") must_== "address"
      squareBrackets.topLevelOnly("address[street]", "person") must_== "address"
      squareBrackets.topLevelOnly("address", "person") must_== "address"
      squareBrackets.topLevelOnly("[address][street]", "person") must_== "address"
      squareBrackets.topLevelOnly("some[person][address][street]", "person") must_== "some"
      squareBrackets.topLevelOnly("some][person][address][street]", "person") must_== "some"
      squareBrackets.topLevelOnly("some]", "person") must_== "some"
    }
  }
}