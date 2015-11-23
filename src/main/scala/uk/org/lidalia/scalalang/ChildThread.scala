package uk.org.lidalia.scalalang

import java.lang.Thread.UncaughtExceptionHandler
import java.util

import org.slf4j.{Logger, LoggerFactory}

class ChildThread extends Thread {

  var parentStack: Option[Array[StackTraceElement]] = None

  setUncaughtExceptionHandler(new UncaughtExceptionHandler {
    override def uncaughtException(t: Thread, e: Throwable): Unit = {
      t.asInstanceOf[ChildThread].fillInStack(e)
      LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME).error("Uncaught exception in thread {}", t: Any, e)
    }
  })

  def fillInStack(e: Throwable): Throwable = {
    val fullStack = Array.concat(e.getStackTrace, parentStack.get)
    e.setStackTrace(fullStack)
    e
  }

  override def start(): Unit = {
    val trace = Thread.currentThread().getStackTrace
    parentStack = Some(util.Arrays.copyOfRange(trace, 2, trace.length))
    super.start()
  }
}
