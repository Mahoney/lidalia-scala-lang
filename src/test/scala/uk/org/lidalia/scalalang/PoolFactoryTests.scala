package uk.org.lidalia.scalalang

import java.lang.System.currentTimeMillis

import org.scalatest.{FunSuite, Outcome, fixture}

import collection.mutable

class PoolFactoryTests extends FunSuite {

  private val resourceFactory = new TestResourceFactory
  val poolFactory = PoolFactory(resourceFactory)

  test("subsequent calls reuse same object") {

    val capturedResources: mutable.Buffer[Resource] = mutable.Buffer()

    poolFactory.using { pool =>

      assert(pool.size == 0)

      pool.using { resource =>
        assert(resource.open)
        capturedResources += resource
      }

      assert(pool.size == 1)

      pool.using { resource =>
        assert(resource.open)
        capturedResources += resource
      }

      assert(pool.size == 1)
    }

    assert(capturedResources.toSet.size == 1)
  }

  test("pool closes resources on exit") {

    var capturedResource: Resource = null

    poolFactory.using { pool =>

      pool.using { resource =>
        capturedResource = resource
      }
    }

    assert(capturedResource.closed)
  }

  test("closing resources happens in constant time") {

    val start = currentTimeMillis()
    poolFactory.using { pool =>

      pool.using { r1 => pool.using { r2 => pool.using { r3 => } } }

    }
    val elapsed = start - currentTimeMillis()

    assert(elapsed < 150L)
  }

  test("pool closes loaned resources on exit") {

    var manualResource: ManuallyClosedResource[Resource] = null
    var capturedResource: Resource = null

    poolFactory.using { pool =>
      manualResource = ManuallyClosedResource(pool)
      manualResource.start()
      capturedResource = manualResource.get()
      assert(capturedResource.open)
    }

    assert(capturedResource.closed)
    manualResource.close()
  }

  test("pool factory closes pool") {

    var captured: Pool[Resource] = null
    poolFactory.using { pool =>
      captured = pool
    }

    assert(captured.closed)
    val illegalState = intercept[IllegalStateException] {
      captured.using { (r) => }
    }

    assert(illegalState.getMessage == "Attempting to use closed pool "+captured)
  }

  test("pool factory throws exception thrown using pool & closes pool") {

    val exception = new RuntimeException("Oh no")
    var captured: Pool[Resource] = null
    val intercepted = intercept[RuntimeException] {
      poolFactory.using { pool =>
        captured = pool
        throw exception
      }
    }

    assert(intercepted == exception)
    assert(captured.closed)

    val illegalState = intercept[IllegalStateException] {
      captured.using { (r) => }
    }

    assert(illegalState.getMessage == "Attempting to use closed pool "+captured)
  }

  test("exception thrown closing resource is propagated") {

    val intercepted = intercept[RuntimeException] {
      PoolFactory(new TestResourceFactory(failOnClose = true)).using { pool =>
          pool.using { resource => }
      }
    }
    assert(intercepted.getMessage == "Failed to close")
  }
}
