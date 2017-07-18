package uk.org.lidalia
package scalalang.process

import sys.process.Process

case class ProcessBuilderExt(command: String) {

  def run(): ProcessExt = {
    val capture = new ProcessOutputCapture
    val process = Process(command).run(capture)
    ProcessExt(process, capture)
  }
}
