package org.singingwizard.genetics.prographs.operations

import org.singingwizard.genetics.prographs.InputValues
import org.singingwizard.genetics.prographs.Operation
import org.singingwizard.genetics.prographs.OutputValues
import org.singingwizard.genetics.prographs.Port
import org.singingwizard.genetics.prographs.TypeInt
import org.singingwizard.genetics.prographs.TypeString
import org.singingwizard.genetics.prographs.Type
import org.singingwizard.genetics.prographs.TypeBoolean

case class Select[T](tpe: Type[T]) extends Operation {
  val Pred = Port("Pred", TypeBoolean)
  val True = Port("True", tpe)
  val False = Port("False", tpe)
  val Out = Port("Out", tpe)
  
  val inputs = IndexedSeq(Pred, True, False)
  val outputs = IndexedSeq(Out)
  
  def run(values: InputValues): OutputValues = {
    OutputValues(Out -> (if(values(Pred)) values(True) else values(False)))
  }
}

case class Switch[T](tpe: Type[T]) extends Operation {
  val Pred = Port("Pred", TypeBoolean)
  val In = Port("True", tpe)
  val True = Port("Out", tpe)
  val False = Port("False", tpe)
  
  val inputs = IndexedSeq(Pred, In)
  val outputs = IndexedSeq(True, False)
  
  def run(values: InputValues): OutputValues = {
    if(values(Pred))
      OutputValues(True -> values(In))
    else
      OutputValues(False -> values(In))
  }
}
