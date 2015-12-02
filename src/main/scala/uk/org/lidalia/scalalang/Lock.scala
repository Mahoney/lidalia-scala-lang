package uk.org.lidalia.scalalang

import java.util.concurrent.locks.ReentrantReadWriteLock

import uk.org.lidalia.scalalang.ResourceFactory._try

final class Lock {

  private val lock = new ReentrantReadWriteLock()

  val readLock = new LockDef(lock.readLock())
  val writeLock = new LockDef(lock.writeLock())

  def using[T](work: => T) = writeLock.using(work)

  final class LockDef private[Lock] (lock: java.util.concurrent.locks.Lock) {
    def using[T](work: => T): T = {
      lock.lock()
      _try {
        work
      } _finally {
        lock.unlock()
      }
    }
  }
}
