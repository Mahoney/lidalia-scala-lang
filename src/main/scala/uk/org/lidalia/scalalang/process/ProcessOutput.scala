package uk.org.lidalia.scalalang.process

case class ProcessOutput(
  status: ProcessStatus,
  out: String,
  err: String,
  all: String
)
