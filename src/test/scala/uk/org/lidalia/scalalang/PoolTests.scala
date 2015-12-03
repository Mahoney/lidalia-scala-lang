package uk.org.lidalia
package scalalang

import org.scalatest.{Outcome, fixture}

class PoolTests extends fixture.FunSuite {

  private val resourceFactory = new TestResourceFactory

  override type FixtureParam = Pool[Resource]

  override protected def withFixture(test: OneArgTest): Outcome = {
    try {
      PoolFactory(resourceFactory).using { pool =>
        test(pool)
      }
    } finally {
      resourceFactory.failOnClose = false
      resourceFactory.failOnOpen = false
      resourceFactory.failOnReset = false
    }
  }

  test("result is propagated") { pool =>

    // when
    val result = pool.using { resource =>
      "expected result"
    }

    // then
    assert(result == "expected result")
  }

  test("pool throws exception thrown using resource and cleans it up") { pool =>

    // when
    var captured: Resource = null

    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        resource.dirty = true
        captured = resource
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(captured.open)
    assert(captured.dirty) // reset was not called
    assert(captured.onErrorCalled.contains(exception))
    assert(pool.size == 1)
  }

  test("exception thrown closing resource after failing to open is suppressed exception") { pool =>

    // given
    resourceFactory.failOnOpen = true
    resourceFactory.failOnClose = true

    // when
    val intercepted = intercept[RuntimeException] {
      pool.using { resource => }
    }

    // then
    assert(intercepted.getMessage == "Failed to open")
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to close"))
    assert(pool.size == 0)
  }

  test("resource is reset after use") { pool =>

    // given
    val dirtyResource = pool.using { resource =>
      resource.dirty = true
      resource
    }

    // expect
    pool.using { resource =>
      assert(resource == dirtyResource)
      assert(!resource.dirty)
    }
  }

  test("pool throws exception thrown resetting resource and closes it") { pool =>

    // given
    resourceFactory.failOnReset = true

    // when
    var captured: Resource = null
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        captured = resource
      }
    }

    // then
    assert(intercepted.getMessage == "Failed to reset")
    assert(captured.closed)
    assert(pool.size == 0)
  }

  test("exception thrown closing resource after exception in reset is suppressed exception") { pool =>

    // given
    resourceFactory.failOnClose = true
    resourceFactory.failOnReset = true

    // when
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
      }
    }

    // then
    assert(intercepted.getMessage == "Failed to reset")
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to close"))
    assert(pool.size == 0)
  }

  test("exception thrown in onError is suppressed exception") { pool =>

    // given
    resourceFactory.failOnReset = true

    // when
    val exception = new RuntimeException("Oh no")
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to handle error"))
    assert(pool.size == 0)
  }

  test("exception thrown in onClose is suppressed by exception in onError which is suppressed exception") { pool =>

    // given
    resourceFactory.failOnClose = true
    resourceFactory.failOnReset = true

    // when
    val exception = new RuntimeException("Oh no")
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to handle error", "Failed to close"))
    assert(pool.size == 0)
  }

  test("if check fails after exception close and eject") { pool =>

    // given
    resourceFactory.onCheck = () => Reusable.BROKEN

    // when
    var captured: Resource = null

    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        resource.dirty = true
        captured = resource
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(captured.closed)
    assert(captured.dirty) // reset was not called
    assert(captured.onErrorCalled.contains(exception))
    assert(pool.size == 0)
  }

  test("if closing after check fails after exception close and eject") { pool =>

    // given
    resourceFactory.onCheck = () => Reusable.BROKEN
    resourceFactory.failOnClose = true

    // when
    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to close"))
    assert(pool.size == 0)
  }

  test("if check throws exception after exception close and eject") { pool =>

    // given
    val checkException = new RuntimeException("Failed check")
    resourceFactory.onCheck = () => throw checkException

    // when
    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed check"))
    assert(pool.size == 0)
  }

  test("if check throws exception and close throws exception after exception propagate suppressed") { pool =>

    // given
    val checkException = new RuntimeException("Failed check")
    resourceFactory.onCheck = () => throw checkException
    resourceFactory.failOnClose = true

    // when
    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    // then
    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed check", "Failed to close"))
    assert(pool.size == 0)
  }

  def flattenSuppressed(throwable: Throwable): Array[Throwable] = {
    throwable.getSuppressed ++ throwable.getSuppressed.flatMap(flattenSuppressed)
  }
}
