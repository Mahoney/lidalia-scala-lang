package uk.org.lidalia.scalalang

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class UnsignedByteTests extends FunSuite with TableDrivenPropertyChecks {

  test("byte conversion") {

    val values = Table(
      ("String value", "Int value", "Signed byte value"),
      (           "0",           0,       Byte.MinValue),
      (         "128",         128,            0.toByte),
      (         "255",         255,       Byte.MaxValue)
    )

    forAll(values) { (stringVal, intVal, signedByteVal) =>

      assert(UnsignedByte(signedByteVal).toString == stringVal)
      assert(UnsignedByte(stringVal).toString == stringVal)
      assert(UnsignedByte(intVal).toString == stringVal)

      assert(UnsignedByte(signedByteVal).toInt == intVal)
      assert(UnsignedByte(stringVal).toInt == intVal)
      assert(UnsignedByte(intVal).toInt == intVal)

      assert(UnsignedByte(signedByteVal).toSignedByte == signedByteVal)
      assert(UnsignedByte(stringVal).toSignedByte == signedByteVal)
      assert(UnsignedByte(intVal).toSignedByte == signedByteVal)

      assert(UnsignedByte(signedByteVal) == UnsignedByte(stringVal))
      assert(UnsignedByte(signedByteVal) == UnsignedByte(intVal))
      assert(UnsignedByte(signedByteVal) == UnsignedByte(signedByteVal))

      assert(UnsignedByte(stringVal) == UnsignedByte(stringVal))
      assert(UnsignedByte(stringVal) == UnsignedByte(intVal))
      assert(UnsignedByte(stringVal) == UnsignedByte(signedByteVal))

      assert(UnsignedByte(intVal) == UnsignedByte(stringVal))
      assert(UnsignedByte(intVal) == UnsignedByte(intVal))
      assert(UnsignedByte(intVal) == UnsignedByte(signedByteVal))
    }
  }

  test("cannot be created with invalid number") {
    val values = Table(
      "Invalid value",
      Integer.MIN_VALUE,
      Integer.MAX_VALUE,
      -1,
      256
    )

    forAll(values) { value =>
      val e = intercept[IllegalArgumentException] {
        UnsignedByte(value)
      }
    }
  }
}
