package uk.org.lidalia.scalalang

import org.scalatest.FunSuite
import uk.org.lidalia.scalalang.ResourceFactory._try

class ResourceFactoryTests extends FunSuite {

  object factory extends ResourceFactory[String] {

    var open = false
    def closed = !open

    override def using[T](work: (String) => T): T = {

      _try {
        open = true
        work("Base string")
      } _finally {
        open = false
      }
    }
  }

  test("withA passes resource to work and returns result") {

    val appended = factory.using { string =>
      assert(factory.open)
      string ++ " appended"
    }

    assert(factory.closed)
    assert(appended == "Base string appended")
  }

  test("withA allows passed resource to be omitted and returns result") {

    val result = factory.using { () =>
      assert(factory.open)
      "result"
    }

    assert(factory.closed)
    assert(result === "result")
  }

  test("safely returns result of work") {
    val result = _try {
      "Result"
    } _finally {
    }
    assert(result == "Result")
  }

  test("safely throws exception in work") {
    val workException = new Throwable("Work failed")
    val thrown = intercept[Throwable] {
      _try {
        throw workException
      } _finally {
      }
    }
    assert(thrown == workException)
    assert(thrown.getSuppressed.isEmpty)
    assert(thrown.getCause == null)
  }

  test("safely throws exception in disposal") {
    val disposalException = new Throwable("Disposal failed")
    val thrown = intercept[Throwable] {
      _try {
        "result"
      } _finally {
        throw disposalException
      }
    }
    assert(thrown == disposalException)
    assert(thrown.getSuppressed.isEmpty)
    assert(thrown.getCause == null)
  }

  test("safely suppresses exception in disposal") {
    val workException = new Throwable("Work failed")
    val disposalException = new Throwable("Disposal failed")
    val thrown = intercept[Throwable] {
      _try {
        throw workException
      } _finally {
        throw disposalException
      }
    }
    assert(thrown == workException)
    assert(thrown.getSuppressed.toList == List(disposalException))
    assert(thrown.getCause == null)
  }
}
