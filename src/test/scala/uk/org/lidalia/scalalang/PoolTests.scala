package uk.org.lidalia.scalalang

import java.lang.System.currentTimeMillis

import org.scalatest.FunSuite
import uk.org.lidalia.scalalang.ResourceFactory._try

import collection.mutable

class PoolTests extends FunSuite {

  val poolFactory = PoolFactory(new TestResourceFactory)

  test("subsequent calls reuse same object") {

    val capturedResources: mutable.Buffer[Resource] = mutable.Buffer()

    poolFactory.using { pool =>

      pool.using { resource =>
        assert(resource.open)
        capturedResources += resource
      }

      pool.using { resource =>
        assert(resource.open)
        capturedResources += resource
      }
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
      capturedResource = manualResource()
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

  test("pool throws exception thrown using resource and closes it") {
    poolFactory.using { pool =>

      val exception = new RuntimeException("Oh no")
      var captured: Resource = null
      val intercepted = intercept[RuntimeException] {
        pool.using { resource =>
          captured = resource
          throw exception
        }
      }

      assert(intercepted == exception)
      assert(captured.closed)

      pool.using { resource =>
        assert(resource != captured)
      }
    }
  }

  test("exception thrown closing resource is propagated") {

    val intercepted = intercept[RuntimeException] {
      PoolFactory(new TestResourceFactory(failOnClose = true)).using { pool =>
          pool.using { resource => }
      }
    }
    assert(intercepted.getMessage == "Failed to close")
  }

  test("exception thrown closing resource after exception is suppressed exception") {

    PoolFactory(new TestResourceFactory(failOnClose = true)).using { pool =>

      val exception = new RuntimeException("Oh no")
      val intercepted = intercept[RuntimeException] {
        pool.using { resource =>
          throw exception
        }
      }

      assert(intercepted == exception)
      assert(intercepted.getSuppressed.map(_.getMessage).toList == List("Failed to close"))
    }
  }

  test("exception thrown closing resource after failing to open is suppressed exception") {

    PoolFactory(new TestResourceFactory(failOnOpen = true, failOnClose = true)).using { pool =>

      val intercepted = intercept[RuntimeException] {
        pool.using { resource =>
        }
      }

      assert(intercepted.getMessage == "Failed to open")
      assert(intercepted.getSuppressed.map(_.getMessage).toList == List("Failed to close"))
    }
  }
}

class TestResourceFactory(failOnOpen: Boolean = false, failOnClose: Boolean = false) extends ResourceFactory[Resource] {
  override def using[T](work: (Resource) => T): T = {
    val resource = new Resource(failOnOpen, failOnClose)
    _try {
      resource.start()
      work(resource)
    } _finally {
      resource.close()
    }
  }
}

class Resource(failOnOpen: Boolean, failOnClose: Boolean) {

  private var _open = false

  def start() = {
    if (failOnOpen) throw new RuntimeException("Failed to open")
    _open = true
  }

  def close() = {
    if (failOnClose) throw new RuntimeException("Failed to close")
    Thread.sleep(50L)
    _open = false
  }

  def open = _open
  def closed = !_open

}
