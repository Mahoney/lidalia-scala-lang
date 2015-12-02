package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

object PoolDefinition {

  def apply[R](resourceFactory: ResourceFactory[R]) = new PoolDefinition(resourceFactory)
}

class PoolDefinition[R] private (resourceFactory: ResourceFactory[R]) extends ResourceFactory[Pool[R]] {

  override def using[T](work: (Pool[R]) => T): T = {
    val pool = new Pool(resourceFactory)
    _try {
      work(pool)
    } _finally {
      pool.close()
    }
  }
}
