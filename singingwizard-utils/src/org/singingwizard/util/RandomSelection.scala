package org.singingwizard.util

import scala.collection
import scala.collection.mutable
import scala.util.Random

object RandomSelection {
  implicit class IterableWithRandom[T](val underlying: collection.Iterable[T]) extends AnyVal {
    def random() = {
      val iter = underlying.iterator
      iter.drop(Random.nextInt(underlying.size))
      val b = iter.next()
      b
    }
  }
  implicit class SetWithTakeRandom[T](val underlying: mutable.Set[T]) extends AnyVal {
    def takeRandom() = {
      val b = underlying.random()
      underlying -= b
      b
    }
  }

  implicit class SeqWithRandom[T](val underlying: collection.Seq[T]) extends AnyVal {
    def randomIndex() = {
      Random.nextInt(underlying.size)
    }
    def random() = {
      underlying(randomIndex())
    }
  }
  implicit class BufferWithTakeRandom[T](val underlying: mutable.Buffer[T]) extends AnyVal {
    def takeRandom() = {
      underlying.remove(underlying.randomIndex())
    }
  }
}