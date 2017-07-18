package uk.org.lidalia.scalalang.process

case class ProcessStatus(status: Int) {
  def isError: Boolean = 0 < status
  def isSuccess: Boolean = status == 0
}
