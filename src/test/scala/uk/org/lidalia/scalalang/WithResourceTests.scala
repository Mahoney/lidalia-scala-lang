package uk.org.lidalia.scalalang

import org.scalatest.{FunSuiteLike, Tag}
import uk.org.lidalia.scalalang.ResourceFactory.usingAll

trait WithResourceTests extends FunSuiteLike {

  def test[A](
    testName: String,
    factory: ResourceFactory[A],
    testTags: Tag*
  )(testFun: (A) => Unit) {
    test(testName, testTags:_*) {
      factory.using { resource =>
        testFun(resource)
      }
    }
  }

  def test[A, B](
    testName: String,
    factory1: ResourceFactory[A],
    factory2: ResourceFactory[B],
    testTags: Tag*
  )(testFun: (A, B) => Unit) {
    test(testName, testTags:_*) {
      usingAll(factory1, factory2) { (res1, res2) =>
        testFun(res1, res2)
      }
    }
  }
}
