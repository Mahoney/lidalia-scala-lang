package uk.org.lidalia
package scalalang

object TryFinally {

  def _try[T](work: => T) = new Finally(work)
}

private [scalalang] class Finally[T](work: => T) {

  def _finally(disposal: => Unit): T = {
    _finally {(ignored) => disposal }
  }

  def _finally(disposal: (?[Throwable]) => Unit): T = {
    var result: ?[T] = None
    try {
      result = Some(work)
    } catch { case t: Throwable =>
      try {
        disposal(t)
      } catch { case t2: Throwable =>
        if (t != t2) t.addSuppressed(t2)
      }
      throw t
    }
    disposal(None)
    result.get
  }
}
