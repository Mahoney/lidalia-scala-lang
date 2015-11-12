package uk.org.lidalia.scalalang

import org.scalatest.FunSuite

class ResourceFactoryTests extends FunSuite {

  object factory extends ResourceFactory[String] {

    var open = false
    def closed = !open

    override def withA[T](work: (String) => T): T = {

      try {
        open = true
        work("Base string")
      } finally {
        open = false
      }
    }
  }

  test("withA passes resource to work and returns result") {

    val appended = factory.withA { string =>
      assert(factory.open)
      string ++ " appended"
    }

    assert(factory.closed)
    assert(appended == "Base string appended")
  }

  test("withA allows passed resource to be omitted and returns result") {

    val result = factory.withA { () =>
      assert(factory.open)
      "result"
    }

    assert(factory.closed)
    assert(result === "result")
  }
}
