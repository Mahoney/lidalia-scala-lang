package uk.org.lidalia.scalalang

object ResourceFactory {

  type RF[R] = ResourceFactory[R]

  def withAll[T, R1, R2](
    rf1: RF[R1], rf2: RF[R2]
  )(
    work: (R1, R2) => T
  ) = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2)
    try {
      work(r1(), r2())
    } finally {
      val allResources = List(r1, r2)
      allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
    }
  }

  def withAll[T, R1, R2, R3](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3]
  )(
    work: (R1, R2, R3) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3)
    try {
      work(r1(), r2(), r3())
    } finally {
      val allResources = List(r1, r2, r3)
      allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
    }
  }

  def withAll[T, R1, R2, R3, R4](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3], rf4: RF[R4]
  )(
    work: (R1, R2, R3, R4) => T
  ): T = {
    val r1 = ManuallyClosedResource(rf1); val r2 = ManuallyClosedResource(rf2); val r3 = ManuallyClosedResource(rf3); val r4 = ManuallyClosedResource(rf4)
    try {
      work(r1(), r2(), r3(), r4())
    } finally {
      val allResources = List(r1, r2, r3, r4)
      allResources.foreach(_.allowToClose())
      allResources.foreach(_.awaitClosed())
    }
  }
}

trait ResourceFactory[R] {

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
