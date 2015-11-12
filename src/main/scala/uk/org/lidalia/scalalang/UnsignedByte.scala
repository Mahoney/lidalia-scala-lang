package uk.org.lidalia.scalalang

import uk.org.lidalia.lang.{RichObject, Identity}

object UnsignedByte {

  val MaxValue = UnsignedByte(Byte.MaxValue)
  val MinValue = UnsignedByte(Byte.MinValue)

  def apply(theByte: Byte): UnsignedByte = new UnsignedByte(theByte)
  def apply(theByte: Int): UnsignedByte = {
    require(theByte >= 0 && theByte < 256)
    UnsignedByte((theByte - 128).toByte)
  }
  def apply(theByte: String): UnsignedByte = UnsignedByte(theByte.toInt)
}

class UnsignedByte private (@Identity private val theByte: Byte) extends RichObject {

  val toInt = theByte+128

  val toSignedByte = theByte

  override val toString: String = String.valueOf(toInt)
}
