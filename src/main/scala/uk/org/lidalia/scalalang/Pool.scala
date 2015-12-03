package uk.org.lidalia.scalalang

import collection.mutable

class Pool[R <: Reusable] private [scalalang] (
  resourceFactory: ResourceFactory[R]
) extends ResourceFactory[R] {

  type ResourceWrapper = ManuallyClosedResource[R]

  private val idle: mutable.Queue[ResourceWrapper] = mutable.Queue()
  private val loaned: mutable.Set[ResourceWrapper] = mutable.Set()
  private val lock = new Lock()

  @volatile var _open = true

  override def using[T](
    work: (R) => T
  ): T = {

    if (!open) {
      throw new IllegalStateException("Attempting to use closed pool "+this)
    }

    val resource = loan

    checkCanOpen(resource)

    val result = doWork(work, resource)

    resetAndRestore(resource)

    result
  }

  private def loan = {

    lock.writeLock.using {

      val existingResource = idle.dequeueFirst((x) => true)

      val result = existingResource.getOrElse(
        ManuallyClosedResource(resourceFactory)
      )
      loaned.add(result)
      result
    }
  }

  private def checkCanOpen[T](
    resourceWrapper: ResourceWrapper
  ) = {
    try {
      resourceWrapper()
    } catch { case e: Exception =>
      eject(resourceWrapper, e)
      throw e
    }
  }

  private def doWork[T](
    work: (R) => T,
    resource: ResourceWrapper
  ) = {
    try {
      work(resource())
    } catch { case e: Exception =>
      handleError(resource, e)
      throw e
    }
  }

  private def resetAndRestore[T](
    resource: ResourceWrapper
  ) = {
    try {
      resource().reset()
      restore(resource)
    } catch { case e: Exception =>
      eject(resource, e)
      throw e
    }
  }

  private def handleError[T](
    resource: ResourceWrapper,
    e: Exception
  ) = {
    try {

      resource().onError(e)

      if (resource().check == Reusable.BROKEN) {
        eject(resource, e)
      } else {
        restore(resource)
      }

    } catch { case e2: Exception =>
        e.addSuppressed(e2)
        eject(resource, e2)
    }
  }

  private def eject[T](
    resource: ResourceWrapper,
    e: Exception
  ) = {
    try {
      lock.writeLock.using {
        loaned.remove(resource)
      }
      resource.close()
    } catch { case e2: Exception =>
      e.addSuppressed(e2)
    }
  }

  private def restore(
    resource: ResourceWrapper
  ) = {
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

  private [scalalang] def close() = {
    _open = false
    idle.foreach(_.allowToClose())
    loaned.foreach(_.allowToClose())
    idle.foreach(_.awaitClosed())
    loaned.foreach(_.awaitClosed())
    idle.clear()
    loaned.clear()
  }
}
