package uk.org.lidalia.scalalang

import java.util.concurrent.locks.ReentrantReadWriteLock

final class Lock {

  private val lock = new ReentrantReadWriteLock()

  val readLock = new LockDef(lock.readLock())
  val writeLock = new LockDef(lock.writeLock())

  final class LockDef private[Lock] (lock: java.util.concurrent.locks.Lock) extends ResourceFactory[Unit] {
    override def withA[T](work: (Unit) => T): T = {
      lock.lock()
      try {
        work(())
      } finally {
        lock.unlock()
      }
    }
  }
}
