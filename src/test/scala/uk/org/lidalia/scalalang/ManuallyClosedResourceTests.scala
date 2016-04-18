package uk.org.lidalia.scalalang

import org.scalatest.FunSuite

class ManuallyClosedResourceTests extends FunSuite {

  test("Returns result") {
    val resource = ManuallyClosedResource(new StubResourceFactory())
    try {
      assert(resource() == "Result")
    } finally {
      resource.close()
    }
  }

  test("throws exception on use") {
    val resource = ManuallyClosedResource(new StubResourceFactory(
      throwWhenOpened = "on use"
    ))

    try {
      val exception = intercept[RuntimeException] {
        resource()
      }
      assert(exception.getMessage == "on use")
      assert(exception.getSuppressed.isEmpty)
      assert(exception.getCause == null)
    } finally {
      resource.close()
    }
  }

  test("throws exception on close") {
    val resource = ManuallyClosedResource(new StubResourceFactory(
      throwWhenClosed = "on close"
    ))
    assert(resource() == "Result")
    val exception = intercept[RuntimeException] {
      resource.close()
    }
    assert(exception.getMessage == "on close")
    assert(exception.getSuppressed.isEmpty)
    assert(exception.getCause == null)
  }
}
