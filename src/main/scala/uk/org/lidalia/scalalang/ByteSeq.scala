package uk.org.lidalia.scalalang

import java.io.{ByteArrayInputStream, InputStream}

import org.apache.commons.io.IOUtils

import scala.collection.immutable

object ByteSeq {

  def apply(bytes: immutable.Seq[UnsignedByte]): ByteSeq = {
    new ByteSeq(bytes)
  }

  def apply(bytes: UnsignedByte*): ByteSeq = {
    new ByteSeq(immutable.Seq(bytes:_*))
  }

  def apply(bytes: Array[Byte]): ByteSeq = {
    new ByteSeq(immutable.Seq(bytes.map(UnsignedByte(_)):_*))
  }

  def apply(input: InputStream): ByteSeq = {
    apply(IOUtils.toByteArray(input))
  }
}

class ByteSeq private (
  bytes: immutable.Seq[UnsignedByte]
) extends immutable.Seq[UnsignedByte] {

  def toSignedArray = bytes.map(_.toSignedByte).toArray
  def toInputStream = new ByteArrayInputStream(toSignedArray)

  override def length: Int = bytes.length

  override def apply(idx: Int): UnsignedByte = bytes.apply(idx)

  override def iterator: Iterator[UnsignedByte] = bytes.iterator
}
