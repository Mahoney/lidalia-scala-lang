package uk.org.lidalia
package scalalang

import java.util.concurrent.CountDownLatch

object ManuallyClosedResource {

  def apply[R](factory: ResourceFactory[R]) = new ManuallyClosedResource(factory)

}
class ManuallyClosedResource[R] private (factory: ResourceFactory[R]) {

  private val ready: CountDownLatch = new CountDownLatch(1)
  private val closed: CountDownLatch = new CountDownLatch(1)

  @volatile
  private var resource: Option[R] = None

  @volatile
  private var throwable: Option[Throwable] = None

  @volatile
  private var closeThrowable: Option[Throwable] = None

  private val thread: Thread = new ChildThread {
    override def run(): Unit = {
      try {
        factory.using { resource =>
          ManuallyClosedResource.this.resource = resource
          ready.countDown()
          closed.await()
        }
      } catch { case e: Throwable =>
        if (ready.getCount > 0) {
          throwable = fillInStack(e)
          ready.countDown()
        } else {
          closeThrowable = fillInStack(e)
        }
      }
    }
  }

  thread.start()

  def apply(): R = {
    ready.await()
    resource.getOrElse(throw throwable.get)
  }

  def close() {
    allowToClose()
    awaitClosed()
    if (closeThrowable.isDefined) throw closeThrowable.get
  }

  def allowToClose() = closed.countDown()

  def awaitClosed() = {
    thread.join()
    if (closeThrowable.isDefined) throw closeThrowable.get
  }

  override def toString = super.toString + '[' + apply + ']'

}
