package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

class TestResourceFactory(
  var failOnOpen: Boolean = false,
  var failOnClose: Boolean = false,
  var failOnReset: Boolean = false,
  var onCheck: () => Reusable.State = () => Reusable.OK
) extends ResourceFactory[Resource] {

  override def using[T](work: (Resource) => T): T = {
    val resource = new Resource(failOnOpen, failOnClose, failOnReset, onCheck)
    _try {
      resource.start()
      work(resource)
    } _finally {
      resource.close()
    }
  }
}
