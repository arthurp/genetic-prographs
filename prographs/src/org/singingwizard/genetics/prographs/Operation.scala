package org.singingwizard.genetics.prographs

import scala.runtime.ScalaRunTime

trait PortTag {
}

case class Port[T](name: String, tpe: Type[T], tags: Set[PortTag] = Set()) {
  type ElementType = T
}

trait OperationBase {
  val inputs: IndexedSeq[AnyPort]
  val outputs: IndexedSeq[AnyPort]

  override def toString() = {
    this match {
      case p: Product ⇒ ScalaRunTime._toString(p)
      case _ ⇒
        val name = getClass().getSimpleName()
        name(name.size - 1) match {
          case '$' ⇒ name.stripSuffix("$")
          case _   ⇒ super.toString()
        }
    }
  }

  def isInput = inputs.isEmpty
  def isOutput = outputs.isEmpty
}

trait Operation extends OperationBase {
  def run(values: Map[AnyPort, Value]): Map[AnyPort, Set[Value]] =
    run(InputValues(values)).values

  def run(values: InputValues): OutputValues
}

case class InputValues(values: Map[AnyPort, Value]) {
  def +[T](p: (Port[T], T)): InputValues = {
    val (port, v) = p
    assert(port.tpe.isInstance(v), s"$port must have type ${port.tpe}. Had value $v.")
    new InputValues(values + (port -> v.asInstanceOf[Value]))
  }

  def apply[T](port: Port[T]): T = {
    values(port).asInstanceOf[T]
  }
}

object InputValues {
  def apply(): InputValues = new InputValues(Map())
  def apply[T](p: (Port[T], T)): InputValues = InputValues() + p
  def apply[T1, T2](p1: (Port[T1], T1), p2: (Port[T2], T2)): InputValues =
    InputValues() + p1 + p2
}

case class OutputValues(val values: Map[AnyPort, Set[Value]]) {
  def +[T](p: (Port[T], T)): OutputValues = {
    val (port, v) = p
    assert(port.tpe.isInstance(v), s"$port must have type ${port.tpe}. Had value $v.")
    val nv = values.getOrElse(port, Set()) + v.asInstanceOf[Value]
    new OutputValues(values + (port -> nv))
  }

  def apply[T](port: Port[T]): Set[port.ElementType] = {
    values(port).asInstanceOf[Set[T]]
  }

  def errorIn(o: OutputValues): Double = {
    val diffs = for((p, vs) <- this.values.toIterable) yield {
      val ov = o(p).asInstanceOf[Set[Value]]
      val diff = (ov - vs) & (vs - ov)
      diff.size / 2.0
    }
    diffs.sum
  }
}

object OutputValues {
  def apply(): OutputValues = new OutputValues(Map())
  def apply[T](p: (Port[T], T)): OutputValues = OutputValues() + p
  def apply[T1, T2](p1: (Port[T1], T1), p2: (Port[T2], T2)): OutputValues =
    OutputValues() + p1 + p2
}