package uk.org.lidalia.scalalang

import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.scalalang.ResourceFactory.{usingAll, _try}

class ResourceFactoryTests extends FunSuite with MockitoSugar with TableDrivenPropertyChecks {

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

  test("using passes resource to work and returns result") {

    val appended = factory.using { string =>
      assert(factory.open)
      string ++ " appended"
    }

    assert(factory.closed)
    assert(appended == "Base string appended")
  }

  test("using allows passed resource to be omitted and returns result") {

    val result = factory.using { () =>
      assert(factory.open)
      "result"
    }

    assert(factory.closed)
    assert(result === "result")
  }

  test("returns result of work") {
    val result = _try {
      "Result"
    } _finally {
    }
    assert(result == "Result")
  }

  test("throws exception in work") {
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

  test("throws exception in disposal") {
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

  test("suppresses exception in disposal") {
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

  val table = Table(
    ("on use 1", "on close 1", "on use 2", "on close 2", "primary message", "suppressed messages"          ),
    ("on use 1", "on close 1", "on use 2", "on close 2", "on use 1",        Set("on close 1")),
    ("on use 1", "on close 1", "on use 2", null        , "on use 1",        Set("on close 1")              ),
    ("on use 1", "on close 1", null,       "on close 2", "on use 1",        Set("on close 2", "on close 1")),
    ("on use 1", "on close 1", null,       null        , "on use 1",        Set("on close 1")              ),

    ("on use 1", null,         "on use 2", "on close 2", "on use 1",        Set("on close 2")              ),
    ("on use 1", null,         "on use 2", null        , "on use 1",        Set()                          ),
    ("on use 1", null,         null,       "on close 2", "on use 1",        Set("on close 2")              ),
    ("on use 1", null,         null,       null        , "on use 1",        Set()                          ),

    (null,       "on close 1", "on use 2", "on close 2", "on use 2",        Set("on close 2", "on close 1")),
    (null,       "on close 1", "on use 2", null        , "on use 2",        Set("on close 1")              ),
    (null,       "on close 1", null,       "on close 2", "on close 2",      Set("on close 1")              ),
    (null,       "on close 1", null,       null        , "on close 1",      Set()                          ),

    (null,       null,         "on use 2", "on close 2", "on use 2",        Set("on close 2")              ),
    (null,       null,         "on use 2", null        , "on use 2",        Set()                          ),
    (null,       null,         null,       "on close 2", "on close 2",      Set()                          )
  )

  forAll(table) { (onUse1, onClose1, onUse2, onClose2, primaryMessage, suppressedMessages) =>
    test(s"usingAll throws exception $onUse1, $onClose1, $onUse2, $onClose2") {
      val resourceFactory1 = new StubResourceFactory("Result", onUse1, onClose1)
      val resourceFactory2 = new StubResourceFactory("Result", onUse2, onClose2)

      val exception = intercept[Exception] {
        usingAll(resourceFactory1, resourceFactory2) { (string1, string2) =>
          string1+string2
        }
      }

      assert(exception.getMessage == primaryMessage)
      assert(exception.getCause == null)
      assert(exception.getSuppressed.map(_.getMessage).toSet == suppressedMessages)
      assert(exception.getSuppressed.flatMap(_.getSuppressed).toList.isEmpty)
    }
  }
}
