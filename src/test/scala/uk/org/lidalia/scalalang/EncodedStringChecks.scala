package uk.org.lidalia.scalalang

import org.scalatest.PropSpecLike
import org.scalatest.prop.TableDrivenPropertyChecks

object EncodedStringChecks extends TableDrivenPropertyChecks with PropSpecLike {

  def checks[A <: EncodedString[A]](factory: EncodedStringFactory[A]) {
    val stringToEncode = new String((0 until 255).map(_.toChar).toArray)
    
    val encoded1 = factory.encode(stringToEncode)
    val encoded2 = factory.encode(stringToEncode)
    val encoded3 = factory.apply(encoded1.toString)
    val encoded4 = factory.apply(encoded1.toString)

    val decoded1 = encoded1.decode
    val decoded2 = encoded2.decode
    val decoded3 = encoded3.decode
    val decoded4 = encoded4.decode

    val reencoded = factory.encode(decoded1)

    val redecoded = reencoded.decode

    assertAllEqual(
      encoded1,
      encoded2,
      encoded3,
      encoded4,
      reencoded
    )

    assertAllEqual(
      encoded1.toString,
      encoded2.toString,
      encoded3.toString,
      encoded4.toString,
      reencoded.toString
    )

    assertAllEqual(
      stringToEncode,
      decoded1,
      decoded2,
      decoded3,
      decoded4,
      redecoded
    )
  }
  
  def assertAllEqual(args: Object*): Unit = {
    forAll(allCombosOf(args.toSeq)) { (arg1, arg2) =>
      assert(arg1 == arg2)
    }
  }

  def allCombosOf[A](args: Seq[A]) = {
    val combos = for (arg1 <- args; arg2 <- args) yield (arg1, arg2)
    Table(
      ("Value 1", "Value 2"),
      combos:_*
    )
  }
}
