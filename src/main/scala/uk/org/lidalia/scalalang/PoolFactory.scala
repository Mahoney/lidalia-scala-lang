package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

object PoolFactory {

  def apply[R <: Reusable](resourceFactory: ResourceFactory[R]) = new PoolFactory(resourceFactory)
}

class PoolFactory[R <: Reusable] private (resourceFactory: ResourceFactory[R]) extends ResourceFactory[Pool[R]] {

  override def using[T](work: (Pool[R]) => T): T = {
    val pool = new Pool(resourceFactory)
    _try {
      work(pool)
    } _finally {
      pool.close()
    }
  }
}
