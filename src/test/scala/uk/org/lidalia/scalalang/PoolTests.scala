package uk.org.lidalia.scalalang

import java.lang.System.currentTimeMillis

import org.scalatest.FunSuite
import uk.org.lidalia.scalalang.ResourceFactory._try

import collection.mutable

class PoolTests extends FunSuite {

  val poolDef = PoolDefinition(new TestResourceFactory)

  test("subsequent calls reuse same object") {

    val capturedResources: mutable.Buffer[Resource] = mutable.Buffer()

    poolDef.using { pool =>

      pool.using { resource =>
        assert(resource.isOpen)
        capturedResources += resource
      }

      pool.using { resource =>
        assert(resource.isOpen)
        capturedResources += resource
      }
    }

    assert(capturedResources.toSet.size == 1)
  }

  test("pool closes resources on exit") {

    var capturedResource: Resource = null

    poolDef.using { pool =>

      pool.using { resource =>
        capturedResource = resource
      }
    }

    assert(capturedResource.isClosed)
  }

  test("closing resources happens in constant time") {

    val start = currentTimeMillis()
    poolDef.using { pool =>

      pool.using { r1 => pool.using { r2 => pool.using { r3 => } } }

    }
    val elapsed = start - currentTimeMillis()

    assert(elapsed < 150L)
  }

  test("closing resources happens in constant time") {

    val start = currentTimeMillis()
    poolDef.using { pool =>

      pool.using { r1 => pool.using { r2 => pool.using { r3 => } } }

    }
    val elapsed = start - currentTimeMillis()

    assert(elapsed < 150L)
  }
}

class TestResourceFactory extends ResourceFactory[Resource] {
  override def using[T](work: (Resource) => T): T = {
    val resource = new Resource
    _try {
      work(resource)
    } _finally {
      resource.close()
    }
  }
}

class Resource {

  private var open = true

  def close() = {
    Thread.sleep(50L)
    open = false
  }

  def isOpen = open
  def isClosed = !open

}
