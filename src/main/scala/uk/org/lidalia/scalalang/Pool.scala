package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

import collection.mutable

class Pool[R] private [scalalang] (resourceFactory: ResourceFactory[R]) extends ResourceFactory[R] {

  private val idle: mutable.Queue[ManuallyClosedResource[R]] = mutable.Queue()
  private val lock = new Lock()

  override def using[T](work: (R) => T): T = {

    val resource = loan

    _try {
      work(resource())
    } _finally {
      lock.writeLock.using { () =>
        idle.enqueue(resource)
      }
    }
  }

  private def loan = {
    val existingResource = lock.writeLock.using { () =>
      idle.dequeueFirst((x) => true)
    }
    existingResource.getOrElse(ManuallyClosedResource(resourceFactory))
  }

  private [scalalang] def close(): Unit = {
    idle.foreach(_.allowToClose())
    idle.foreach(_.awaitClosed())
  }
}
