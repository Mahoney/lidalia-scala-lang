package uk.org.lidalia.scalalang

import uk.org.lidalia.?

object Throwables {

  def suppressed(throwables: Traversable[Throwable]): ?[Throwable] = {
    throwables.headOption.foreach { primary =>
      throwables.tail.foreach { primary.addSuppressed }
    }
    throwables.headOption
  }
}
