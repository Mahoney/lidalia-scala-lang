package uk.org.lidalia.net2

object Authority {
  def apply(authorityStr: String): Authority = {
    val userInfoHostAndPort = authorityStr.split("@", 2)
    if (userInfoHostAndPort.size == 2) {
      Authority(
        UserInfo(userInfoHostAndPort(0)),
        HostAndPort(userInfoHostAndPort(1))
      )
    } else {
      Authority(
        hostAndPort = HostAndPort(userInfoHostAndPort(0))
      )
    }
  }

  def apply(userInfo: ?[UserInfo] = None, hostAndPort: HostAndPort)
    = new Authority(userInfo, hostAndPort)
}

class Authority private(val userInfo: ?[UserInfo],
                        val hostAndPort: HostAndPort) {

  override def toString = userInfo.map(_+"@"+hostAndPort) or hostAndPort.toString


  def canEqual(other: Any): Boolean = other.isInstanceOf[Authority]

  override def equals(other: Any): Boolean = other match {
    case that: Authority =>
      (that canEqual this) &&
          userInfo == that.userInfo &&
          hostAndPort == that.hostAndPort
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(userInfo, hostAndPort)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
