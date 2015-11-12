package uk.org.lidalia.scalalang

import java.util.regex.Pattern

import uk.org.lidalia.lang.WrappedValue

abstract class RegexVerifiedWrappedString(value: CharSequence, verifier: Pattern) extends WrappedValue(value) {
  require(
    verifier.matcher(value).matches(),
    s"${getClass.getSimpleName} [$value] must match ${verifier.pattern}"
  )
}
