package uk.org.lidalia.scalalang

import org.scalatest.PropSpecLike
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

object EqualsChecks extends TableDrivenPropertyChecks with PropSpecLike {

  def equalsTest[A,B](argCombos: List[A])(constructor: ((A) => B)*) {
    val instanceCombosTable = buildConstructorArgCombinationTable(argCombos)

    forAll(instanceCombosTable) { (args1, args2, shouldBeEqual) =>
      val instance1 = constructor(0)(args1)
      val instance2 = constructor(0)(args2)
      assert((instance1 == instance2) === shouldBeEqual)
      assert((instance2 == instance1) === shouldBeEqual)
      assert((instance1.hashCode == instance2.hashCode) === shouldBeEqual)
    }
  }

  def reflexiveTest[A,B](argCombos: List[A])(constructor: (A) => B) {
    val argCombosTable =
      Table(
        "Constructor args",
        argCombos:_*
      )

    forAll(argCombosTable) { (args) =>
      val instance = constructor(args)
      assert(instance == instance)
    }
  }

  def possibleArgsFor[A](arg1s: List[A]): List[(A)] = {
    for (arg1 <- arg1s) yield (arg1)
  }

  def possibleArgsFor[A,B](arg1s: List[A], arg2s: List[B]): List[(A, B)] = {
    for (arg1 <- arg1s; arg2 <- arg2s) yield (arg1, arg2)
  }

  def possibleArgsFor[A,B,C](arg1s: List[A], arg2s: List[B], argumentList3: List[C]): List[(A,B,C)] = {
    for (arg1 <- arg1s; arg2 <- arg2s; arg3 <- argumentList3) yield (arg1, arg2, arg3)
  }

  def possibleArgsFor[A,B,C,D](arg1s: List[A], arg2s: List[B], argumentList3: List[C], argumentList4: List[D]): List[(A,B,C,D)] = {
    for (arg1 <- arg1s; arg2 <- arg2s; arg3 <- argumentList3; arg4 <- argumentList4) yield (arg1, arg2, arg3, arg4)
  }

  def buildConstructorArgCombinationTable[A](possibleConstructorArgs: List[A]): TableFor3[A, A, Boolean] = {
    val x = for (constructorArgs1 <- possibleConstructorArgs; constructorArgs2 <- possibleConstructorArgs) yield (constructorArgs1, constructorArgs2, constructorArgs1 == constructorArgs2)
    Table(
      ("Constructor args for 1", "Constructor args for 2", "Should they be Equal"),
      x: _*
    )
  }
}
