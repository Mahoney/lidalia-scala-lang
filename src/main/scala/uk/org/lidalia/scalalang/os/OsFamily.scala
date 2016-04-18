package uk.org.lidalia.scalalang.os

sealed trait OsFamily
case object Windows extends OsFamily
case object Linux extends OsFamily
case object Mac extends OsFamily
case object Other extends OsFamily

object OsFamily {

  def apply(): OsFamily = {
    apply(System.getProperty("os.name"))
  }

  def apply(osNameMixed: String): OsFamily = {
    val osName = osNameMixed.toLowerCase
    if (osName.startsWith("mac")) {
      Mac
    } else if (osName.startsWith("linux")) {
      Linux
    } else if (osName.startsWith("win")) {
      Windows
    } else {
      Other
    }
  }
}
