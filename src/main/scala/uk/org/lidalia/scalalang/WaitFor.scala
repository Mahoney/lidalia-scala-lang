package uk.org.lidalia.scalalang

import java.time.Duration
import java.time.Instant.now
import java.util.concurrent.TimeoutException

object WaitFor {

  def waitFor(description: String = "", timeout: Duration = Duration.ofSeconds(5), pollInterval: Duration = Duration.ofMillis(50))(predicate: => Boolean) = {

    val start = now()

    def testPredicate(): Unit = {
      if (!predicate) {
        if (now().isAfter(start.plus(timeout))) {
          throw new TimeoutException(s"Condition $description did not pass within $timeout")
        }
        Thread.sleep(pollInterval.toMillis)
        testPredicate()
      }
    }

    testPredicate()
  }
}
