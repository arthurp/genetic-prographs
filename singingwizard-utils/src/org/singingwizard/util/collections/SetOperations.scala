package org.singingwizard.util.collections

object SetOperations {
  implicit class SetWithAdditional[T](val underlying: collection.Set[T]) extends AnyVal {
    def symDiff(o: collection.Set[T]): collection.Set[T] = (underlying diff o) & (o diff underlying)
  }
}