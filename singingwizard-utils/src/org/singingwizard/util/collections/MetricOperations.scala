package org.singingwizard.util.collections

import scala.math.pow
import scala.math.sqrt

object MetricOperations {
  implicit class SetAddDiffVector[T](val underlying: collection.Set[T]) extends AnyVal {
    def differenceVector[N: Numeric](o: collection.Set[T], distance: N) = {
      (underlying ++ o).toSeq.map { e ⇒
        if (underlying(e) == o(e)) {
          implicitly[Numeric[N]].zero
        } else {
          distance
        }
      }
    }

    def differenceVector[N: Numeric](o: collection.Set[T], distance: (T) ⇒ N) = {
      (underlying ++ o).toSeq.map { e ⇒
        if (underlying(e) == o(e)) {
          implicitly[Numeric[N]].zero
        } else {
          distance(e)
        }
      }
    }
  }

  implicit class DoubleSeqWithAdditional(val underlying: collection.Seq[Double]) {
    def pNorm(p: Double) = {
      if (p == Double.PositiveInfinity || p == Double.NegativeInfinity) {
        underlying.max
      } else if (p == 1) {
        underlying.sum
      } else if (p == 2) {
        sqrt(underlying.map(x ⇒ x * x).sum)
      } else {
        pow(underlying.map(pow(_, p)).sum, 1 / p)
      }
    }
  }

  implicit class IntSeqWithAdditional(val underlying: collection.Seq[Int]) {
    def pNorm(p: Double): Double = {
      if (p == Double.PositiveInfinity || p == Double.NegativeInfinity) {
        underlying.max
      } else if (p == 1) {
        underlying.sum
      } else if (p == 2) {
        sqrt(underlying.map(x ⇒ x * x).sum)
      } else {
        pow(underlying.map(pow(_, p)).sum, 1 / p)
      }
    }
  }

  implicit class BooleanWithAdditional(val underlying: Boolean) extends AnyVal {
    def ?>[N: Numeric](v: N) = if (underlying) v else implicitly[Numeric[N]].one
    def !?>[N: Numeric](v: N) = if (!underlying) v else implicitly[Numeric[N]].zero
  }

}