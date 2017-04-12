package org.singingwizard.genetics.prographs.evolution

import org.singingwizard.genetics.prographs.{ InputValues, OutputValues }
import org.singingwizard.genetics.prographs.interpreter.IOInterpreter
import org.singingwizard.genetics.prographs.{ AnyBlock, AnyPort, Value }
import org.singingwizard.genetics.prographs.interpreter.IOOperation
import scala.collection.mutable

object Evaluator {
}

case class EvaluationResult(correctSamples: Double, usedBlocks: Double, totalBlocks: Int, blockInvocations: Int)

abstract class Evaluator {
  type Sample = (InputValues, OutputValues)
  def samples: List[Sample]

  def apply(ind: Genotype): EvaluationResult = {
    val interpreter = new IOInterpreter(ind.program) {
      var blockInvocations = 0
      val blocksUsed = new mutable.HashSet[AnyBlock]()

      override protected def invokeBlock(block: AnyBlock, inputs: IndexedSeq[(AnyPort, Value)]) = {
        blocksUsed += block
        blockInvocations += 1
        super.invokeBlock(block, inputs)
      }
    }
    
    val errors = for((in, out) <- samples) yield {
      val computedResult = interpreter.run(in)
      out.errorIn(computedResult)
    }
    
    val totalBlocks = ind.program.blocks.size
    
    EvaluationResult(errors.sum, interpreter.blocksUsed.size.toDouble / totalBlocks, totalBlocks, interpreter.blockInvocations)
  }
}