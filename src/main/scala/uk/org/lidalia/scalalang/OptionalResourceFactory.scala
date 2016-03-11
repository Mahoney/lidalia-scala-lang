package uk.org.lidalia
package scalalang

class OptionalResourceFactory[R] private (
  useResource: Boolean,
  factory: ResourceFactory[R]
) extends ResourceFactory[?[R]] {

  override def using[T](work: (?[R]) => T): T = {
    if (useResource) {
      factory.using { resource =>
        work(Option(resource))
      }
    } else {
      work(None)
    }
  }
}

object OptionalResourceFactory {
  def apply[R](
                useDisplay: Boolean,
                displayFactory: ResourceFactory[R]
                ) = {
    new OptionalResourceFactory(useDisplay, displayFactory)
  }
}
