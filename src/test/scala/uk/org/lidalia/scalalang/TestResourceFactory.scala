package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

class TestResourceFactory(
  var failOnOpen: Boolean = false,
  var failOnClose: Boolean = false,
  var failOnReset: Boolean = false
) extends ResourceFactory[Resource] {

  override def using[T](work: (Resource) => T): T = {
    val resource = new Resource(failOnOpen, failOnClose, failOnReset)
    _try {
      resource.start()
      work(resource)
    } _finally {
      resource.close()
    }
  }
}
