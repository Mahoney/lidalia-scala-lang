package uk.org.lidalia
package scalalang

class Resource(
  failOnOpen: Boolean,
  failOnClose: Boolean,
  failOnReset: Boolean
) extends Reusable {

  private var _open = false

  def start() = {
    if (failOnOpen) throw new RuntimeException("Failed to open")
    _open = true
  }

  def close() = {
    if (failOnClose) throw new RuntimeException("Failed to close")
    Thread.sleep(50L)
    _open = false
  }

  def open = _open
  def closed = !_open

  var dirty = false
  var onErrorCalled: ?[Exception] = None

  override def reset(): Unit = {
    if (failOnReset) throw new RuntimeException("Failed to reset")
    dirty = false
  }

  override def onError(exception: Exception): Unit = {
    if (failOnReset) throw new RuntimeException("Failed to handle error")
    onErrorCalled = exception
  }
}
