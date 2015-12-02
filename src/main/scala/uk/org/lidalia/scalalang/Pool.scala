package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

import collection.mutable

class Pool[R] private [scalalang] (resourceFactory: ResourceFactory[R]) extends ResourceFactory[R] {

  private val idle: mutable.Queue[ManuallyClosedResource[R]] = mutable.Queue()
  private val loaned: mutable.Set[ManuallyClosedResource[R]] = mutable.Set()
  private val lock = new Lock()

  @volatile var _open = true

  override def using[T](work: (R) => T): T = {

    if (!open) throw new IllegalStateException("Attempting to use closed pool "+this)

    val resource = loan

    _try {
      work(resource())
    } _finally { maybeE =>
      if (maybeE.isDefined) eject(resource)
      else _return(resource)
    }
  }

  private def loan = {
    val existingResource = lock.writeLock.using {
      idle.dequeueFirst((x) => true)
    }
    val result = existingResource.getOrElse(ManuallyClosedResource(resourceFactory))
    lock.writeLock.using {
      loaned.add(result)
    }
    result
  }

  private def _return(resource: ManuallyClosedResource[R]): Unit = {
    lock.writeLock.using {
      idle.enqueue(resource)
      loaned.remove(resource)
    }
  }

  private def eject(resource: ManuallyClosedResource[R]): Unit = {
    lock.writeLock.using {
      loaned.remove(resource)
    }
    resource.close()
  }

  def open = _open
  def closed = !open

  private [scalalang] def close(): Unit = {
    _open = false
    idle.foreach(_.allowToClose())
    loaned.foreach(_.allowToClose())
    idle.foreach(_.awaitClosed())
    loaned.foreach(_.awaitClosed())
  }
}
