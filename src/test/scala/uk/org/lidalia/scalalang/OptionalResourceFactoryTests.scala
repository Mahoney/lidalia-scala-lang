package uk.org.lidalia.scalalang

import org.scalatest.FunSuite

import util.Random

class OptionalResourceFactoryTests extends FunSuite {

  test("creates and makes available a resource if turned on") {

    val resource = Random.nextInt()
    val factory = stubFactory(resource)
    val expectedOutcome = Random.nextInt()

    val outcome = OptionalResourceFactory(useDisplay = true, factory).using { optionalResource =>
      (optionalResource, expectedOutcome)
    }

    assert(outcome == (Some(resource), expectedOutcome))
    assert(factory.calls == 1)
  }

  test("makes no resource if turned off") {

    val factory = stubFactory(Random.nextInt())
    val expectedOutcome = Random.nextInt()

    val outcome = OptionalResourceFactory(useDisplay = false, factory).using { optionalResource =>
      (optionalResource, expectedOutcome)
    }

    assert(outcome == (None, expectedOutcome))
    assert(factory.calls == 0)
  }

  def stubFactory[R](resource: R) = new ResourceFactory[R] {
    var calls = 0
    override def using[T](work: (R) => T): T = {
      calls = calls + 1
      work(resource)
    }
  }
}
