package uk.org.lidalia.scalalang

object ResourceFactoryAdapter {

  def apply[R](resource: R) = new ResourceFactoryAdapter(resource)
  def asFactory[R](resource: R) = apply(resource)
}

class ResourceFactoryAdapter[+R] private (
  resource: R
) extends ResourceFactory[R] {

  override def using[T](work: (R) => T): T = work(resource)

}
