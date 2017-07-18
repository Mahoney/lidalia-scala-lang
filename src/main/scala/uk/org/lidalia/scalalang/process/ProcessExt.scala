package uk.org.lidalia.scalalang.process

import sys.process.Process

case class ProcessExt (process: Process, capture: ProcessOutputCapture) {
  def destroy(): Unit = {
    process.destroy()
  }
  def output(): ProcessOutput = {
    val status = process.exitValue()
    ProcessOutput(ProcessStatus(status), capture.outStr, capture.errStr, capture.allStr)
  }
}
