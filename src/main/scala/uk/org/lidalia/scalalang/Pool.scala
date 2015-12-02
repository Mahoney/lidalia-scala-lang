package uk.org.lidalia.scalalang

import uk.org.lidalia.scalalang.ResourceFactory._try

import collection.mutable

class Pool[R <: Reusable] private [scalalang] (resourceFactory: ResourceFactory[R]) extends ResourceFactory[R] {

  private val idle: mutable.Queue[ManuallyClosedResource[R]] = mutable.Queue()
  private val loaned: mutable.Set[ManuallyClosedResource[R]] = mutable.Set()
  private val lock = new Lock()

  @volatile var _open = true

  override def using[T](work: (R) => T): T = {

    if (!open) throw new IllegalStateException("Attempting to use closed pool "+this)

    val resourceWrapper = loan

    _try {
      val resource = resourceWrapper()
      val result = _try {
        work(resource)
      } _finally { maybeE =>
        maybeE match {
          case Some(e) => resource.onError(e)
          case _ =>
        }
      }
      resource.reset()
      result
    } _finally { maybeE =>
      if (maybeE.isDefined) {
        eject(resourceWrapper)
      } else {
        _return(resourceWrapper)
      }
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

  def size = {
    lock.readLock.using {
      idle.size + loaned.size
    }
  }

  private [scalalang] def close(): Unit = {
    _open = false
    idle.foreach(_.allowToClose())
    loaned.foreach(_.allowToClose())
    idle.foreach(_.awaitClosed())
    loaned.foreach(_.awaitClosed())
  }
}
