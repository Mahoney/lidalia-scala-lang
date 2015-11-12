package uk.org.lidalia.scalalang

/**
 * Represents a String in encoded form - for instance Base64 encoded or Hex encoded
 * 
 * Use toString to retrieve the encoded representation and decode to retrieve the decoded
 * representation.
 *
 * Create with [[EncodedStringFactory.apply]] if you already have an encoded representation or
 * [[EncodedStringFactory.encode()]] if you do not.
 *
 * Immutable
 * 
 * @tparam T The self type - extending classes should use their own type here
 */
trait EncodedString[T <: EncodedString[T]] extends CharSequence {

  /**
   * @return the encoded representation of the String
   */
  override def toString: String

  /**
   * @return the decoded representation of the String
   */
  def decode: String

  /**
   * @return the [[EncodedStringFactory]] that can be used to create further instances of this encoded type
   */
  val factory: EncodedStringFactory[T]

  /**
   * @param toAppendUnencoded an unencoded String to encode and append to the current String
   * @return a new EncodedString composed of this and an encoded form of the passed String
   */
  def ++(toAppendUnencoded: String): T = factory(toString+factory.encode(toAppendUnencoded))

  /**
   * @param toAppend an encoded String to append to the current String
   * @return a new EncodedString composed of this and the passed EncodedString
   */
  def ++(toAppend: T): T = factory(toString+toAppend.toString)

  def length = toString.length

  def charAt(index: Int) = toString.charAt(index)

  def subSequence(start: Int, end: Int): T = factory.apply(toString.substring(start, end))
}

/**
 * Allows creation of [[EncodedString]]s from an already encoded or an unencoded String
 *
 * @tparam T the type of the EncodedString
 */
trait EncodedStringFactory[T <: EncodedString[T]] {

  /**
   * Creates a container for an encoded string.
   * Use [[EncodedString.toString]] to retrieve the original encoded value.
   *
   * @param encoded A plain String in already encoded form
   * @throws IllegalArgumentException if the encoded String is not in a valid encoded form
   * @return an [[EncodedString]] wrapping the encoded value
   */
  def apply(encoded: String): T

  /**
   * Encodes a string.
   * Use [[EncodedString.decode]] to retrieve the original unencoded value.
   *
   * @param unencoded A plain unencoded String
   * @return an [[EncodedString]] representing the encoded value of the unencoded String
   */
  def encode(unencoded: String): T

}
