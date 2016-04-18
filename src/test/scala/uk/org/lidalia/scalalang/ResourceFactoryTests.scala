package uk.org.lidalia
package scalalang

import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.org.lidalia.scalalang.ResourceFactory.{_try, usingAll}

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

  val exceptionCombinations = Table(
    ("on open 1", "on close 1", "on open 2", "on close 2", "expected"          ),

    ("on open 1", "on close 1", "on open 2", "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on open 2", suppressed = List(
                                                               ExceptionExpectation("on close 2")
                                                             ))
                                                           ))),
    ("on open 1", "on close 1", "on open 2", null        , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on open 2")
                                                           ))),
    ("on open 1", "on close 1", null,        "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    ("on open 1", "on close 1", null,        null         , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1")
                                                           ))),

    ("on open 1", null,         "on open 2", "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on open 2", suppressed = List(
                                                               ExceptionExpectation("on close 2")
                                                             ))
                                                           ))),
    ("on open 1", null,         "on open 2", null        , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on open 2")
                                                           ))),
    ("on open 1", null,         null,        "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    ("on open 1", null,         null,        null        , ExceptionExpectation("on open 1")),

    (null,        "on close 1", "on open 2", "on close 2", ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 2"),
                                                             ExceptionExpectation("on close 1")
                                                           ))),
    (null,        "on close 1", "on open 2", null        , ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 1")
                                                           ))),
    (null,        "on close 1", null,        "on close 2", ExceptionExpectation("on close 1", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    (null,        "on close 1", null,        null        , ExceptionExpectation("on close 1")),

    (null,        null,         "on open 2", "on close 2", ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    (null,        null,         "on open 2", null        , ExceptionExpectation("on open 2")),
    (null,        null,         null,        "on close 2", ExceptionExpectation("on close 2"))
  )

  forAll(exceptionCombinations) { (onUse1, onClose1, onUse2, onClose2, expected) =>
    test(s"usingAll correctly suppresses exceptions when work would succeed: $onUse1, $onClose1, $onUse2, $onClose2") {
      val resourceFactory1 = StubResourceFactory("resource1", onUse1, onClose1)
      val resourceFactory2 = StubResourceFactory("resource2", onUse2, onClose2)

      val exception = intercept[Exception] {
        usingAll(resourceFactory1, resourceFactory2) { (string1, string2) =>
          // do nothing
        }
      }

      assert(ExceptionExpectation(exception) == expected)
    }
  }

  val exceptionCombinationsWhenWorkFails = Table(
    ("on open 1", "on close 1", "on open 2", "on close 2", "expected"          ),

    ("on open 1", "on close 1", "on open 2", "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on open 2", suppressed = List(
                                                               ExceptionExpectation("on close 2")
                                                             ))
                                                           ))),
    ("on open 1", "on close 1", "on open 2", null        , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on open 2")
                                                           ))),
    ("on open 1", "on close 1", null,        "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    ("on open 1", "on close 1", null,        null         , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 1")
                                                           ))),

    ("on open 1", null,         "on open 2", "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on open 2", suppressed = List(
                                                               ExceptionExpectation("on close 2")
                                                             ))
                                                           ))),
    ("on open 1", null,         "on open 2", null        , ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on open 2")
                                                           ))),
    ("on open 1", null,         null,        "on close 2", ExceptionExpectation("on open 1", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    ("on open 1", null,         null,        null        , ExceptionExpectation("on open 1")),

    (null,        "on close 1", "on open 2", "on close 2", ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 2"),
                                                             ExceptionExpectation("on close 1")
                                                           ))),
    (null,        "on close 1", "on open 2", null        , ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 1")
                                                           ))),
    (null,        "on close 1", null,        "on close 2", ExceptionExpectation("work failed", suppressed = List(
                                                             ExceptionExpectation("on close 1"),
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    (null,        "on close 1", null,        null        , ExceptionExpectation("work failed", suppressed = List(
                                                             ExceptionExpectation("on close 1")
                                                           ))),

    (null,        null,         "on open 2", "on close 2", ExceptionExpectation("on open 2", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           ))),
    (null,        null,         "on open 2", null        , ExceptionExpectation("on open 2")),
    (null,        null,         null,        "on close 2", ExceptionExpectation("work failed", suppressed = List(
                                                             ExceptionExpectation("on close 2")
                                                           )))
  )

  forAll(exceptionCombinationsWhenWorkFails) { (onUse1, onClose1, onUse2, onClose2, expected) =>
    test(s"usingAll correctly suppresses exceptions when work fails: $onUse1, $onClose1, $onUse2, $onClose2") {
      val resourceFactory1 = StubResourceFactory("resource1", onUse1, onClose1)
      val resourceFactory2 = StubResourceFactory("resource2", onUse2, onClose2)

      val exception = intercept[Exception] {
        usingAll(resourceFactory1, resourceFactory2) { (string1, string2) =>
          throw new Exception("work failed")
        }
      }

      assert(ExceptionExpectation(exception) == expected)
    }
  }
}

object ExceptionExpectation {
  def apply(t: Throwable): ExceptionExpectation = {
    new ExceptionExpectation(
      t.getMessage,
      Option(t.getCause).map(ExceptionExpectation(_)),
      t.getSuppressed.map(ExceptionExpectation(_)).toList
    )
  }
}

case class ExceptionExpectation(message: String, cause: ?[ExceptionExpectation] = None, suppressed: List[ExceptionExpectation] = Nil)
