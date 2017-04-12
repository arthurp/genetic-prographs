package org.singingwizard.genetics.prographs

import org.singingwizard.util.collections.mutable.BidiMap
import org.singingwizard.util.collections.RandomSelection._
import org.singingwizard.util.collections.SetOperations._
import org.singingwizard.util.collections.MetricOperations._
import org.singingwizard.genetics.prographs.interpreter.IOInputBase
import org.singingwizard.genetics.prographs.interpreter.IOOutputBase
import collection.mutable

trait GraphAlignment { this: Graph ⇒
  private def comparePorts(a: PortOnBlock[_], b: PortOnBlock[_]): Double = {
    comparePorts(a.port, b.port)
  }
  private def comparePorts(a: AnyPort, b: AnyPort): Double = {
    val distanceVector = Seq(a.tpe == b.tpe !?> 3, a.name == b.name !?> 2) ++ a.tags.differenceVector(b.tags, 1)
    distanceVector.pNorm(1)
  }

  def align(other: Graph): (Set[(AnyBlock, AnyBlock)], Set[(AnyConnection, AnyConnection)]) = {
    val blockAlignment = new BidiMap[AnyBlock, AnyBlock]()
    val connectionAlignment = new BidiMap[AnyConnection, AnyConnection]()

    var perimeterBlocks = new BidiMap[AnyBlock, AnyBlock]()

    // Seed the alignment with inputs and outputs.
    for (tb ← this.blocks if tb.operation.isInstanceOf[IOInputBase[_]]) {
      val obs = other.blocks.collect {
        case ob if ob.operation.isInstanceOf[IOInputBase[_]] &&
          ob.operation.asInstanceOf[IOInputBase[_]].port == tb.operation.asInstanceOf[IOInputBase[_]].port ⇒
          ob
      }
      val ob: AnyBlock = obs.random()
      blockAlignment += tb -> ob
      ()
    }

    for (tb ← this.blocks if tb.operation.isInstanceOf[IOOutputBase[_]]) {
      val obs = other.blocks.collect {
        case ob if ob.operation.isInstanceOf[IOOutputBase[_]] &&
          ob.operation.asInstanceOf[IOOutputBase[_]].port == tb.operation.asInstanceOf[IOOutputBase[_]].port ⇒
          ob
      }
      val ob: AnyBlock = obs.random()
      blockAlignment += tb -> ob
      ()
    }

    perimeterBlocks ++= blockAlignment.leftToRight

    pprint.pprintln(blockAlignment.leftToRight)

    def alignPerimeter() = {
      val startPeri = perimeterBlocks
      perimeterBlocks = new BidiMap[AnyBlock, AnyBlock]()
      for ((l, r) ← startPeri.leftToRight) {
        val remainingConnectionsL = 
        pprint.pprintln(blockAlignment.leftToRight)
      }
    }

    ???
  }
}