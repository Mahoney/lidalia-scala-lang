package uk.org.lidalia.scalalang

import org.scalacheck.Gen

import scala.util.Random

object TestUtils {

  def genRandomStringFrom(stringGenerators: Gen[String]*): Gen[String] = {
    def genNode(level: Int): Gen[String] = for {
      start <- Gen.lzy(genRandomString(level))
      end <- Gen.lzy(genRandomString(level))
    } yield start + end

    def genRandomString(level: Int): Gen[String] = {
      val of: Gen[String] = Gen.oneOf(
        stringGenerators.head,
        stringGenerators.tail.head,
        stringGenerators.tail.tail:_*
      )
      if (level >= 100) of
      else {
        Gen.oneOf(of, genNode(level + 1))
      }
    }

    genRandomString(0)
  }

  def genStringFromChars(chars: Set[Char]) =
    for (n <- Gen.someOf(chars))
    yield n.mkString

  def genNonEmptyStringFromChars(chars: Set[Char]) =
    for (n <- genStringFromChars(chars))
    yield if (n.isEmpty) ""+Random.shuffle(chars).head else n
}
