package uk.org.lidalia.scalalang

import collection.mutable

class Pool[R <: Reusable] private [scalalang] (resourceFactory: ResourceFactory[R]) extends ResourceFactory[R] {

  private val idle: mutable.Queue[ManuallyClosedResource[R]] = mutable.Queue()
  private val loaned: mutable.Set[ManuallyClosedResource[R]] = mutable.Set()
  private val lock = new Lock()

  @volatile var _open = true

  override def using[T](work: (R) => T): T = {

    if (!open) throw new IllegalStateException("Attempting to use closed pool "+this)

    val resourceWrapper = loan

    val resource = unwrap(resourceWrapper)

    val result = doWork(work, resourceWrapper, resource)

    returnResource(resourceWrapper, resource)

    result
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

  private def unwrap[T](resourceWrapper: ManuallyClosedResource[R]): R = {
    try {
      resourceWrapper()
    } catch {
      case e: Exception =>
        eject(resourceWrapper, e)
        throw e
    }
  }

  private def doWork[T](work: (R) => T, resourceWrapper: ManuallyClosedResource[R], resource: R): T = {
    try {
      work(resource)
    } catch {
      case e: Exception =>
        handleError(resourceWrapper, resource, e)
        throw e
    }
  }

  private def returnResource[T](resourceWrapper: ManuallyClosedResource[R], resource: R): Unit = {
    try {
      resource.reset()
      _return(resourceWrapper)
    } catch {
      case e: Exception =>
        eject(resourceWrapper, e)
        throw e
    }
  }

  private def handleError[T](resourceWrapper: ManuallyClosedResource[R], resource: R, e: Exception): Unit = {
    try {
      resource.onError(e)
      _return(resourceWrapper)
    } catch {
      case e2: Exception =>
        e.addSuppressed(e2)
        eject(resourceWrapper, e2)
    }
  }

  private def eject[T](resource: ManuallyClosedResource[R], e: Exception): Unit = {
    try {
      lock.writeLock.using {
        loaned.remove(resource)
      }
      resource.close()
    } catch {
      case e2: Exception =>
        e.addSuppressed(e2)
    }
  }

  private def _return(resource: ManuallyClosedResource[R]): Unit = {
    lock.writeLock.using {
      idle.enqueue(resource)
      loaned.remove(resource)
    }
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
