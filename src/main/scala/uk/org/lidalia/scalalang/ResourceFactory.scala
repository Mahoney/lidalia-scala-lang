package uk.org.lidalia
package scalalang

import uk.org.lidalia.scalalang.ResourceFactory.usingAll

import scala.util.Try

object ResourceFactory {

  type RF[R] = ResourceFactory[R]

  def usingAll[T, R1, R2](
    rf1: RF[R1], rf2: RF[R2]
  )(
    work: (R1, R2) => T
  ) = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2)
    _try {
      (Try(r1()), Try(r2()))
      work(r1(), r2())
    } _finally { maybe =>
      val allResources = List(r1, r2)
      val exceptions = close2(allResources)
      maybe.orElse(exceptions.headOption).foreach { t =>
        exceptions.drop(1).foreach { t2 => t.addSuppressed(t2) }
        throw t
      }
    }
  }

  def usingAll[T, R1, R2, R3](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3]
  )(
    work: (R1, R2, R3) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3)
    _try {
      work(r1(), r2(), r3())
    } _finally {
      val allResources = List(r1, r2, r3)
      close(allResources)
    }
  }

  def usingAll[T, R1, R2, R3, R4](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3], rf4: RF[R4]
  )(
    work: (R1, R2, R3, R4) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3); val r4 = ManuallyClosedResource(rf4)
    _try {
      work(r1(), r2(), r3(), r4())
    } _finally {
      val allResources = List(r1, r2, r3, r4)
      close(allResources)
    }
  }

  private def close(allResources: List[ManuallyClosedResource[_]]): Unit = {
    val exceptions = close2(allResources)
    exceptions.headOption.foreach { t =>
      exceptions.tail.foreach { s => t.addSuppressed(s) }
      throw t
    }
  }

  def close2(allResources: List[ManuallyClosedResource[_]]): List[Throwable] = {
    allResources.foreach(_.allowToClose())
    val tries: List[Try[Unit]] = allResources.map { it => Try(it.awaitClosed()) }
    val failures: List[Try[Unit]] = tries.filter(_.isFailure)
    val exceptions = failures.map(_.failed.get)
    exceptions
  }

  def usingAll[T, R](factories: ResourceFactory[R]*)(work: List[R] => T) = {
    val resources = factories.map(ManuallyClosedResource(_))
    _try {
      work(resources.map(_.apply()).toList)
    } _finally {
      resources.foreach(_.allowToClose())
      resources.foreach(_.awaitClosed())
    }
  }

  def _try[T](work: => T) = new Finally(work)

}

private [scalalang] class Finally[T](work: => T) {

  def _finally(disposal: (?[Throwable]) => Unit): T = {
    var result: ?[T] = None
    try {
      result = Some(work)
    } catch {
      case t: Throwable =>
        try {
          disposal(t)
        } catch {
          case t2: Throwable =>
            t.addSuppressed(t2)
        }
        throw t
    }
    disposal(None)
    result.get
  }

  def _finally(disposal: => Unit): T = {
    _finally({ (ignored) => disposal })
  }
}

trait ResourceFactory[+R] {

  def map[T](work: (R) => T) = using(work)

  def using[T](work: (R) => T): T

  def using[T](work: () => T): T = {
    using((ignore) => work())
  }
}

object Reusable extends Enumeration {
  type State = Value
  val BROKEN, OK, CLOSED, DIRTY = Value
}

trait Reusable {

  def check: Reusable.State = {
    Reusable.OK
  }

  def onError(): Unit = {
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

class ExistingResourceFactory[+R](existingResource: R) extends ResourceFactory[R] {
  override def using[T](work: (R) => T) = work(existingResource)
}
