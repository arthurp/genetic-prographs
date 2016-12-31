package org.singingwizard.genetics.prographs.interpreter

import org.singingwizard.genetics.prographs.{ AnyBlock, AnyPort, Graph, InputValues, Operation, OutputValues, Port, Type, Value }

class IOInterpreter(graph: Graph) extends Interpreter(graph) {
  var inputs = InputValues()
  var outputs = OutputValues()

  override protected def invokeBlock(block: AnyBlock, inputs: IndexedSeq[(AnyPort, Value)]) = {
    block.operation match {
      case o: IOOperation ⇒ o.run(this, inputs.toMap[AnyPort, Value])
      case o              ⇒ o.run(inputs.toMap[AnyPort, Value])
    }
  }
}

trait IOOperation extends Operation {
  def run(interpreter: IOInterpreter, values: Map[AnyPort, Value]): Map[AnyPort, Set[Value]] =
    run(interpreter, InputValues(values)).values

  def run(interpreter: IOInterpreter, values: InputValues): OutputValues

  def run(values: InputValues): OutputValues =
    throw new UnsupportedOperationException(
      s"$this only supports executing in an IOInterpreter")
}

case class IOOutput[T](port: Port[T]) extends IOOperation {
  val In = Port("In", port.tpe)

  val inputs = IndexedSeq(In)
  val outputs = IndexedSeq()

  def run(interpreter: IOInterpreter, values: InputValues): OutputValues = {
    interpreter.outputs += (port -> values(In))
    OutputValues()
  }
}

case class IOInput[T](port: Port[T]) extends IOOperation {
  val Out = Port("Out", port.tpe)

  val inputs = IndexedSeq()
  val outputs = IndexedSeq(Out)

  def run(interpreter: IOInterpreter, values: InputValues): OutputValues = {
    OutputValues(Out -> interpreter.inputs(port))
  }
}

case class UITriggeredInput[T, T2](port: Port[T], tpe2: Type[T2]) extends IOOperation {
  val Trigger = Port("Trigger", tpe2)
  val Out = Port("Out", port.tpe)

  val inputs = IndexedSeq(Trigger)
  val outputs = IndexedSeq(Out)

  def run(interpreter: IOInterpreter, values: InputValues): OutputValues = {
    OutputValues(Out -> interpreter.inputs(port))
  }
}