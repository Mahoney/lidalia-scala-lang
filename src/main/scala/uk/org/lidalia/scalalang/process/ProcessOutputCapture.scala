package uk.org.lidalia.scalalang.process

import collection.mutable.ListBuffer
import sys.process.ProcessLogger

private [process] class ProcessOutputCapture extends ProcessLogger {

  private val outLines = ListBuffer[String]()
  private val errLines = ListBuffer[String]()
  private val allLines = ListBuffer[String]()

  override def out(s: => String): Unit = {
    outLines.append(s)
    allLines.append(s)
  }

  override def err(s: => String): Unit = {
    errLines.append(s)
    allLines.append(s)
  }

  def outStr = outLines.mkString(System.lineSeparator)

  def errStr = errLines.mkString(System.lineSeparator)

  def allStr = allLines.mkString(System.lineSeparator)

  override def buffer[T](f: => T): T = f

}
