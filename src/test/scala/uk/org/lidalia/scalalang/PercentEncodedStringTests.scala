package uk.org.lidalia.scalalang

import org.scalatest.FunSuite

class PercentEncodedStringTests extends FunSuite {

  val allChars = new ConcretePercentEncodedStringFactory(Set())
  val fNotEncoded = new ConcretePercentEncodedStringFactory(Set('f', 'F'))

  test("can encode all characters") {
    
    val toEncode = "\n\t"+"""aA0%-\/[]{}()^$!@£#$&*!~`|?<>,."""

    assert(allChars.encode(toEncode).toString === "%0a%09%61%41%30%25%2d%5c%2f%5b%5d%7b%7d%28%29%5e%24%21%40%a3%23%24%26%2a%21%7e%60%7c%3f%3c%3e%2c%2e")

  }
  
  test("standard encoded string checks") {
    EncodedStringChecks.checks(allChars)
  }
  
  test("case ignored in hex digits") {
    assert(allChars("%ff") === allChars("%FF"))
  }

  test("case expected in other digits") {
    assert(fNotEncoded("ff") !== fNotEncoded("FF"))
  }

  test("equals checks for factory") {

    EqualsChecks.equalsTest(List(Set[Char](), Set('a'), Set('a', 'A'))) { args =>
      new ConcretePercentEncodedStringFactory(args)
    }

    EqualsChecks.reflexiveTest(List(Set[Char](), Set('a'), Set('a', 'A'))) { args =>
      new ConcretePercentEncodedStringFactory(args)
    }
  }

  test("not equal when one is encoded and other is not") {
    assert(fNotEncoded("f") !== fNotEncoded("%66"))
  }

  test("not equal when have different factories") {
    assert(fNotEncoded("%66") !== allChars("%66"))
  }
}
