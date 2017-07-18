package uk.org.lidalia.scalalang

import java.util.Locale

import org.scalatest.{Outcome, TestSuite}

trait DefaultLocale extends TestSuite {

  val defaultLocale: Locale

  override abstract def withFixture(test : NoArgTest): Outcome = {
    val oldDefault = Locale.getDefault
    Locale.setDefault(defaultLocale)
    try {
      super.withFixture(test)
    } finally {
      Locale.setDefault(oldDefault)
    }
  }
}
