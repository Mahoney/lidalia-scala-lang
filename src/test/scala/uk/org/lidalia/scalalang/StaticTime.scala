package uk.org.lidalia.scalalang

import org.joda.time.{DateTimeUtils, Instant}
import org.scalatest.{Outcome, Suite}

trait StaticTime extends Suite {

  val staticTime: Instant

  override abstract def withFixture(test : NoArgTest): Outcome = {
    DateTimeUtils.setCurrentMillisFixed(staticTime.getMillis)
    try {
      super.withFixture(test)
    } finally {
      DateTimeUtils.setCurrentMillisSystem()
    }
  }
}
