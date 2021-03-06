package uk.org.lidalia
package scalalang

import scalalang.ResourceFactory.usingAll
import scalalang.Throwables.suppressed
import scalalang.TryFinally._try

import scala.util.Try

object ResourceFactory {

  type RF[R] = ResourceFactory[R]

  def usingAll[T, R1, R2](
    rf1: RF[R1], rf2: RF[R2]
  )(
    work: (R1, R2) => T
  ) = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2)
    _usingAll(r1, r2) {
      work(r1.get(), r2.get())
    }
  }

  def usingAll[T, R1, R2, R3](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3]
  )(
    work: (R1, R2, R3) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3)
    _usingAll(r1, r2, r3) {
      work(r1.get(), r2.get(), r3.get())
    }
  }

  def usingAll[T, R1, R2, R3, R4](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3], rf4: RF[R4]
  )(
    work: (R1, R2, R3, R4) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3); val r4 = ManuallyClosedResource(rf4)
    _usingAll(r1, r2, r3, r4) {
      work(r1.get(), r2.get(), r3.get(), r4.get())
    }
  }

  def usingAll[T, R](factories: ResourceFactory[R]*)(work: Seq[R] => T): T = {
    val resources = factories.map(ManuallyClosedResource(_))
    _usingAll(resources:_*) {
      work(resources.map(_.get()))
    }
  }

  private def _usingAll[T](allResources: ManuallyClosedResource[_]*)(work: => T): T = {
    _try {
      allResources.foreach(_.start())
      val exceptionsOnOpen = exceptionsOn(allResources.toList) { _.get() }
      suppressed(exceptionsOnOpen).foreach { throw _ }
      work
    } _finally { t: ?[Throwable] =>
      closeAll(allResources.toList, t)
    }
  }

  private def exceptionsOn[T](allResources: Traversable[T])(action: (T) => Any) = {
    allResources.flatMap { r => Try(action(r)).failed.toOption }
  }

  private def closeAll(allResources: Traversable[ManuallyClosedResource[_]], t: ?[Throwable]): Unit = {
    allResources.foreach(_.allowToClose())
    val exceptionsOnClose = exceptionsOn(allResources) { _.awaitClosed() }
    val allExceptions = t.map { _ :: exceptionsOnClose.toList }.getOrElse(exceptionsOnClose)
    suppressed(allExceptions).foreach { throw _ }
  }

  def usingAllSerial[T, R](factories: ResourceFactory[R]*)(work: Seq[R] => T): T = {
    val allResources = factories.map(ManuallyClosedResource(_))
    _try {
      val exceptionsOnOpen = exceptionsOn(allResources.toList) { r => r.start(); r.get() }
      suppressed(exceptionsOnOpen).foreach { throw _ }
      work(allResources.map(_.get()))
    } _finally { t: ?[Throwable] =>
      val exceptionsOnClose = exceptionsOn(allResources.reverse) { _.close() }
      val allExceptions = t.map { _ :: exceptionsOnClose.toList }.getOrElse(exceptionsOnClose)
      suppressed(allExceptions).foreach { throw _ }
    }
  }
}

trait ResourceFactory[+R] {

  def using[T](work: (R) => T): T

  def using[T](work: () => T): T = {
    using((ignore) => work())
  }
}

object Reusable extends Enumeration {
  type State = Value
  val BROKEN, OK = Value
}

trait Reusable {

  def check: Reusable.State = {
    Reusable.OK
  }

  def onError(exception: Exception): Unit = {
    reset()
  }

  def reset(): Unit = {}
}

class MultiResourceFactory2[+A, +B](
  rf1: ResourceFactory[A], rf2: ResourceFactory[B]
) extends ResourceFactory[(A, B)] {

  override def using[T](work: ((A, B)) => T): T = {
    usingAll(rf1, rf2) { (r1, r2) =>
      work((r1, r2))
    }
  }
}

class MultiResourceFactory3[+A, +B, +C](
  rf1: ResourceFactory[A], rf2: ResourceFactory[B], rf3: ResourceFactory[C]
) extends ResourceFactory[(A, B, C)] {

  override def using[T](work: ((A, B, C)) => T): T = {
    usingAll(rf1, rf2, rf3) { (r1, r2, r3) =>
      work((r1, r2, r3))
    }
  }
}

class MultiResourceFactory4[+A, +B, +C, +D](
  rf1: ResourceFactory[A], rf2: ResourceFactory[B], rf3: ResourceFactory[C], rf4: ResourceFactory[D]
) extends ResourceFactory[(A, B, C, D)] {

  override def using[T](work: ((A, B, C, D)) => T): T = {
    usingAll(rf1, rf2, rf3, rf4) { (r1, r2, r3, r4) =>
      work((r1, r2, r3, r4))
    }
  }
}

object ExistingResourceFactory {
  def apply[R](existingResource: R) = new ExistingResourceFactory(existingResource)
}

class ExistingResourceFactory[+R] private (existingResource: R) extends ResourceFactory[R] {
  override def using[T](work: (R) => T) = work(existingResource)
}

class CloseableResourceFactory[+R <: AutoCloseable](builder: () => R) extends ResourceFactory[R] {
  override def using[T](work: (R) => T): T = {
    val closeable = builder()
    _try {
      work(closeable)
    } _finally {
      closeable.close()
    }
  }
}
