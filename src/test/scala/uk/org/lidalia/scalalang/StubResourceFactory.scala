package uk.org.lidalia.scalalang

import ResourceFactory._try

case class StubResourceFactory[R] (
  resource: R = "Result",
  throwWhenOpened: String = null,
  throwWhenClosed: String = null
) extends ResourceFactory[R] {

  override def using[T](work: (R) => T): T = {
    _try {
      if (throwWhenOpened != null) throw new RuntimeException(throwWhenOpened)
      work(resource)
    } _finally {
      if (throwWhenClosed != null) throw new RuntimeException(throwWhenClosed)
    }
  }
}
