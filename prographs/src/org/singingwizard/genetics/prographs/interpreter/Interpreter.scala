package org.singingwizard.genetics.prographs.interpreter

import scala.collection.mutable

import org.singingwizard.genetics.prographs.{ AnyBlock, AnyPort, Graph, PortOnBlock, Value }
import org.singingwizard.util.collections.RandomSelection._

class Interpreter(graph: Graph) {
  def run(): Unit = {
    val readyBlocks = new mutable.HashSet[AnyBlock]()
    val channels = new mutable.HashMap[PortOnBlock[_], mutable.Buffer[Value]]()

    def getChannel(p: PortOnBlock[_]): mutable.Buffer[Value] = {
      channels.getOrElseUpdate(p, new mutable.ListBuffer())
    }

    readyBlocks ++= graph.inputBlocks

    while (readyBlocks.nonEmpty) {
      Logger.finest(s"Executing with readyBlocks: $readyBlocks")

      val block = readyBlocks.takeRandom()

      val inputs = for (p ← block.inputs) yield {
        p.port -> getChannel(p).takeRandom()
      }
      Logger.finest(s"Executing block $block with input $inputs")
      val outputs = invokeBlock(block, inputs)
      Logger.fine(s"Executing block $block with input $inputs and output $outputs")

      val listeningBlocks = (for ((p, v) ← outputs) yield {
        val ports = graph.listeningPorts(PortOnBlock(block, p))
        ports foreach { pp ⇒ getChannel(pp) ++= v }
        ports map { _.block }
      }).flatten.toSet

      val newReadyBlocks = listeningBlocks.filter(_.inputs.forall(p ⇒ getChannel(p).nonEmpty))

      Logger.fine(s"New blocks ready: $newReadyBlocks")
      newReadyBlocks foreach { readyBlocks += _ }
    }
  }

  protected def invokeBlock(block: AnyBlock, inputs: IndexedSeq[(AnyPort, Value)]) = {
    block.operation.run(inputs.toMap[AnyPort, Value])
  }
}