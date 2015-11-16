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

  private val thread: Thread = new Thread {
    override def run(): Unit = {
      factory.using { resource =>
        ManuallyClosedResource.this.resource = resource
        ready.countDown()
        closed.await()
      }
    }
  }

  thread.start()

  def apply(): R = {
    ready.await()
    resource.get
  }

  def close() {
    allowToClose()
    awaitClosed()
  }

  def allowToClose() = closed.countDown()

  def awaitClosed() = thread.join()

  override def toString = super.toString + '[' + apply + ']'

}
