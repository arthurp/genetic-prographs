package org.singingwizard.util.collections

import scala.util.Random
import scala.collection.generic.CanBuildFrom

object RandomSelection {
  implicit class IterableWithRandom[T](val underlying: collection.Iterable[T]) extends AnyVal {
    def random() = {
      val iter = underlying.iterator
      iter.drop(Random.nextInt(underlying.size))
      val b = iter.next()
      b
    }
  }
  implicit class SetWithTakeRandom[T](val underlying: collection.mutable.Set[T]) extends AnyVal {
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
    def shuffle() = {
      val buf = underlying.toBuffer
      val n = buf.size
      // for i from 0 to n−2 do
      for (i ← 0 to n - 2) {
        // j ← random integer such that i ≤ j < n
        val j = Random.nextInt(n - i) + i
        // exchange a[i] and a[j]
        val tmp = buf(i)
        buf(i) = buf(j)
        buf(j) = tmp
      }
      
      buf.toSeq
    }
  }
  implicit class BufferWithTakeRandom[T](val underlying: collection.mutable.Buffer[T]) extends AnyVal {
    def takeRandom() = {
      underlying.remove(underlying.randomIndex())
    }
  }
}