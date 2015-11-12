package uk.org.lidalia.scalalang

import java.util.Locale

import org.scalatest.{Outcome, Suite}

trait DefaultLocale extends Suite {

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
