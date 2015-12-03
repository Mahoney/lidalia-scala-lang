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
    val result = pool.using { resource =>
      "expected result"
    }

    assert(result == "expected result")
  }

  test("pool throws exception thrown using resource and cleans it up") { pool =>

    var captured: Resource = null

    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        resource.dirty = true
        captured = resource
        throw exception
      }
    }

    assert(intercepted == exception)
    assert(captured.open)
    assert(captured.dirty) // reset was not called
    assert(captured.onErrorCalled.contains(exception))
    assert(pool.size == 1)

    pool.using { resource =>
      assert(resource == captured)
    }
  }

  test("exception thrown closing resource after failing to open is suppressed exception") { pool =>

    resourceFactory.failOnOpen = true
    resourceFactory.failOnClose = true

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
      }
    }

    assert(intercepted.getMessage == "Failed to open")
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to close"))
    assert(pool.size == 0)
  }

  test("resource is reset after use") { pool =>

    var captured: Resource = null

    pool.using { resource =>
      resource.dirty = true
      captured = resource
    }

    pool.using { resource =>
      assert(resource == captured)
      assert(!resource.dirty)
    }
  }

  test("pool throws exception thrown resetting resource and closes it") { pool =>

    resourceFactory.failOnReset = true

    var captured: Resource = null
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        captured = resource
      }
    }

    assert(intercepted.getMessage == "Failed to reset")
    assert(captured.closed)
    assert(pool.size == 0)
  }

  test("exception thrown closing resource after exception in reset is suppressed exception") { pool =>

    resourceFactory.failOnClose = true
    resourceFactory.failOnReset = true

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
      }
    }

    assert(intercepted.getMessage == "Failed to reset")
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to close"))
    assert(pool.size == 0)
  }

  test("exception thrown in onError is suppressed exception") { pool =>

    resourceFactory.failOnReset = true

    val exception = new RuntimeException("Oh no")
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to handle error"))
    assert(pool.size == 0)
  }

  test("exception thrown in onClose is suppressed by exception in onError which is suppressed exception") { pool =>

    resourceFactory.failOnClose = true
    resourceFactory.failOnReset = true

    val exception = new RuntimeException("Oh no")
    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        throw exception
      }
    }

    assert(intercepted == exception)
    assert(flattenSuppressed(intercepted).map(_.getMessage).toList == List("Failed to handle error", "Failed to close"))
    assert(pool.size == 0)
  }

  test("if check fails after exception close and eject") { pool =>

    var captured: Resource = null

    val exception = new RuntimeException("Oh no")

    val intercepted = intercept[RuntimeException] {
      pool.using { resource =>
        resource.dirty = true
        captured = resource
        throw exception
      }
    }

    assert(intercepted == exception)
    assert(captured.open)
    assert(captured.dirty) // reset was not called
    assert(captured.onErrorCalled.contains(exception))
    assert(pool.size == 1)

    pool.using { resource =>
      assert(resource == captured)
    }
  }

  def flattenSuppressed(throwable: Throwable): Array[Throwable] = {
    throwable.getSuppressed ++ throwable.getSuppressed.flatMap(flattenSuppressed)
  }
}
