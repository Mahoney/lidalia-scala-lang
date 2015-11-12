package uk.org

import annotation.meta.field
import scala.language.implicitConversions

package object lidalia {

  type Identity = uk.org.lidalia.lang.Identity @field

  type ?[T] = Option[T]

  implicit def instanceToSome[T](instance: T): Some[T] = Some(instance)
  implicit def someToInstance[T](some: Some[T]): T = some.get

}
