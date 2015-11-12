package uk.org.lidalia.scalalang

object ResourceFactory {

  type RF[R] = ResourceFactory[R]

  def withAll[T, R1, R2](
    rf1: RF[R1], rf2: RF[R2]
  )(
    work: (R1, R2) => T
  ) = {
    rf1.withA { r1 => rf2.withA { r2 =>
      work(r1, r2)
    } }
  }

  def withAll[T, R1, R2, R3](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3]
  )(
    work: (R1, R2, R3) => T
  ): T = {
    rf1.withA { r1 => rf2.withA { r2 => rf3.withA { r3 =>
      work(r1, r2, r3)
    } } }
  }

  def withAll[T, R1, R2, R3, R4](
    rf1: RF[R1], rf2: RF[R2], rf3: RF[R3], rf4: RF[R4]
  )(
    work: (R1, R2, R3, R4) => T
  ): T = {
    rf1.withA { r1 => rf2.withA { r2 => rf3.withA { r3 => rf4.withA { r4 =>
      work(r1, r2, r3, r4)
    } } } }
  }
}

trait ResourceFactory[R] {

  def withA[T](work: (R) => T): T

  def withA[T](work: () => T): T = {
    withA((ignore) => work())
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
