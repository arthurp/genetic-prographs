package org.singingwizard.genetics.prographs.operations

import org.singingwizard.genetics.prographs.{ InputValues, Operation, OutputValues, Port, Type, TypeInt, TypeString }

object Add extends Operation {
  val A = Port("A", TypeInt)
  val B = Port("B", TypeInt)
  val Sum = Port("Sum", TypeInt)

  val inputs = IndexedSeq(A, B)
  val outputs = IndexedSeq(Sum)

  def run(values: InputValues): OutputValues = {
    OutputValues(Sum -> (values(A) + values(B)))
  }
}

object Concat extends Operation {
  val A = Port("A", TypeString)
  val B = Port("B", TypeString)
  val Sum = Port("Sum", TypeString)

  val inputs = IndexedSeq(A, B)
  val outputs = IndexedSeq(Sum)

  def run(values: InputValues): OutputValues = {
    OutputValues(Sum -> (values(A) + values(B)))
  }
}

object IntToString extends Operation {
  val In = Port("In", TypeInt)
  val Out = Port("Out", TypeString)

  val inputs = IndexedSeq(In)
  val outputs = IndexedSeq(Out)

  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> values(In).toString)
  }
}

object Log extends Operation {
  val In = Port("In", TypeString)

  val inputs = IndexedSeq(In)
  val outputs = IndexedSeq()

  def run(values: InputValues): OutputValues = {
    println(values(In))
    OutputValues()
  }
}

case class Constant[T](tpe: Type[T], v: T) extends Operation {
  val Out = Port("Out", tpe)

  val inputs = IndexedSeq()
  val outputs = IndexedSeq(Out)

  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> v)
  }
}

case class TriggeredConstant[T](tpe: Type[T], v: T, tpe2: Type[T]) extends Operation {
  val Trigger = Port("Trigger", tpe2)
  val Out = Port("Out", tpe)

  val inputs = IndexedSeq(Trigger)
  val outputs = IndexedSeq(Out)

  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> v)
  }
}
