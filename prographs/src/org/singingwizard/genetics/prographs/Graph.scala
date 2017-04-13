package org.singingwizard.genetics.prographs

import org.singingwizard.util.{ DotableEdge, DotableGraph, DotableNode }
import scala.reflect.ClassTag

class Block[+Op <: Operation: ClassTag](val name: String, val operation: Op) extends DotableNode {
  override def toString() = {
    val id = System.identityHashCode(this).toHexString
    s"Block:$name@$id(${operation})"
  }

  def apply[T](port: Port[T]) = {
    require((operation.inputs contains port) || (operation.outputs contains port))
    PortOnBlock(this, port)
  }

  def inputs = operation.inputs.map(PortOnBlock(this, _))
  def outputs = operation.outputs.map(PortOnBlock(this, _))

  def dotName: String = {
    val id = System.identityHashCode(this).toHexString
    s"Block_$id"
  }

  def toDot(): String = {
    s"""subgraph cluster_$dotName { label="$name"; color=black; 
      ${inputs.map(_.toDot()).mkString("\n")}
      ${outputs.map(_.toDot()).mkString("\n")}
      }"""
  }
}

object Block {
  implicit def blockAsOperation[Op <: Operation](b: Block[Op]): Op = b.operation
}

case class PortOnBlock[T](block: AnyBlock, port: Port[T]) extends DotableNode {
  def -->(other: PortOnBlock[T]) = {
    Connection(this, other)
  }

  def dotName: String = {
    val id = hashCode.toHexString
    s"PortOnBlock_$id"
  }

  def toDot(): String = {
    s"""$dotName [label="${port.name}"];"""
  }
}

case class Connection[T](src: PortOnBlock[T], dst: PortOnBlock[T]) extends DotableEdge {
  require(src.port.tpe == dst.port.tpe,
    s"Connections must have the same type on both ends; ports ${src.port} -> ${dst.port}")

  def tpe = src.port.tpe

  def dotName: String = {
    val id = hashCode.toHexString
    s"Connection_$id"
  }

  def toDot(): String = {
    s"""${src.dotName} -> ${dst.dotName};"""
    //[taillabel="${src.port.name}", headlabel="${dst.port.name}"]
  }
}

case class Graph(blocks: Set[AnyBlock] = Set(), connections: Set[AnyConnection] = Set()) extends DotableGraph with GraphAlignment with GraphCutting {
  require(connections forall { c ⇒ (blocks contains c.src.block) && (blocks contains c.dst.block) },
    s"Graph contains a connection to a block not in this graph.")

  def +(c: AnyConnection) = Graph(blocks + c.src.block + c.dst.block, connections + c)
  def +(b: AnyBlock) = Graph(blocks + b, connections)

  override def toString() = {
    s"""Graph{
${blocks.mkString("\n")}
=======
${connections.map(c ⇒ s"${c.src.block.name}.${c.src.port.name} --> ${c.dst.block.name}.${c.dst.port.name}").mkString("\n")}
}
"""
  }

  def dotName = s"Graph${hashCode.toHexString}"

  def toDot() = {
    s"""
      digraph $dotName {
        ${blocks.map(_.toDot()).mkString("\n")}
        ${connections.map(_.toDot()).mkString("\n")}
      }
      """
  }

  def listeningPorts[T](p: PortOnBlock[T]): Set[PortOnBlock[T]] = {
    for (c ← connections if c.src == p) yield {
      c.dst.asInstanceOf[PortOnBlock[T]]
    }
  }

  def inputBlocks = blocks.filter(_.operation.isInput)
  def outputBlocks = blocks.filter(_.operation.isOutput)
}

object Graph {
  def apply(c1: AnyConnection, connections: AnyConnection*): Graph = {
    connections.foldLeft(Graph() + c1)(_ + _)
  }
}