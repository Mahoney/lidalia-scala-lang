package uk.org.lidalia.scalalang

import ResourceFactory._try

class StubResourceFactory (
  result: String = "Result",
  throwWhenUsed: String = null,
  throwWhenClosed: String = null
) extends ResourceFactory[String] {

  override def using[T](work: (String) => T): T = {
    _try {
      if (throwWhenUsed != null) throw new RuntimeException(throwWhenUsed)
      work(result)
    } _finally {
      if (throwWhenClosed != null) throw new RuntimeException(throwWhenClosed)
    }
  }
}
