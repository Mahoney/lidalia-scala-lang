package uk.org.lidalia.net2

import uk.org.lidalia.lang.WrappedValue

object Fragment {
  def apply(fragmentStr: String) = new Fragment(fragmentStr)
}

class Fragment private(override val toString: String) extends AnyVal
