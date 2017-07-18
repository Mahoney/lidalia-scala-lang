package uk.org.lidalia.scalalang

object NoneResourceFactory extends ResourceFactory[None.type] {
  override def using[T](work: (None.type) => T) = work(None)
}

class SomeResoureFactory[R](factory: ResourceFactory[R]) extends ResourceFactory[Some[R]] {
  override def using[T](work: (Some[R]) => T) = {
    factory.using { resource =>
      work(Some(resource))
    }
  }
}

object SomeResoureFactory {
  def apply[R](factory: ResourceFactory[R]) = new SomeResoureFactory(factory)
}
