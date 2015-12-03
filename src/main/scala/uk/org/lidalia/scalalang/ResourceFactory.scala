package uk.org.lidalia
package scalalang

import uk.org.lidalia.scalalang.ResourceFactory.usingAll

object ResourceFactory {

  type RF[R] = ResourceFactory[R]

  def usingAll[T, R1, R2](
    rf1: RF[R1], rf2: RF[R2]
  )(
    work: (R1, R2) => T
  ) = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2)
    _try {
      work(r1(), r2())
    } _finally {
      val allResources = List(r1, r2)
      allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
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
      allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
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
    allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
    }
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

  def _finally(disposal: => Unit): T = {
    var result: ?[T] = None
    try {
      result = Some(work)
    } catch {
      case t: Throwable =>
        try {
          disposal
        } catch {
          case t2: Throwable =>
            t.addSuppressed(t2)
        }
        throw t
    }
    disposal
    result.get
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

class ExistingResourceFactory[+R](existingResource: R) extends ResourceFactory[R] {
  override def using[T](work: (R) => T) = work(existingResource)
}
