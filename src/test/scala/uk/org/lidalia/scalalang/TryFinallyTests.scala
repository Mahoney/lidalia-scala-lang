package uk.org.lidalia.scalalang

import org.scalatest.FunSuite
import uk.org.lidalia.scalalang.TryFinally._try

class TryFinallyTests extends FunSuite {

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
        _throw(disposalException)
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
        _throw(disposalException)
      }
    }
    assert(thrown == workException)
    assert(thrown.getSuppressed.toList == List(disposalException))
    assert(thrown.getCause == null)
  }

  test("ignores exception in disposal if same as one in work") {
    val workException = new Throwable("Work failed")
    val thrown = intercept[Throwable] {
      _try {
        throw workException
      } _finally {
        _throw(workException)
      }
    }
    assert(thrown == workException)
    assert(thrown.getSuppressed.toList.isEmpty)
    assert(thrown.getCause == null)
  }

  def _throw(t: Throwable): Unit = throw t
}
