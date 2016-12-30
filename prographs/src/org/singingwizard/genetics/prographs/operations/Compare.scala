package org.singingwizard.genetics.prographs.operations

import org.singingwizard.genetics.prographs.InputValues
import org.singingwizard.genetics.prographs.Operation
import org.singingwizard.genetics.prographs.OutputValues
import org.singingwizard.genetics.prographs.Port
import org.singingwizard.genetics.prographs.TypeInt
import org.singingwizard.genetics.prographs.TypeString
import org.singingwizard.genetics.prographs.Type
import org.singingwizard.genetics.prographs.TypeBoolean

object LessThan extends Operation {
  val A = Port("A", TypeInt)
  val B = Port("B", TypeInt)
  val Out = Port("Out", TypeBoolean)
  
  val inputs = IndexedSeq(A, B)
  val outputs = IndexedSeq(Out)
  
  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> (values(A) < values(B)))
  }
}

case class Equals[T](tpe: Type[T]) extends Operation {
  val A = Port("True", tpe)
  val B = Port("False", tpe)
  val Out = Port("Out", TypeBoolean)
  
  val inputs = IndexedSeq(A, B)
  val outputs = IndexedSeq(Out)
  
  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> (values(A) == values(B)))
  }
}
