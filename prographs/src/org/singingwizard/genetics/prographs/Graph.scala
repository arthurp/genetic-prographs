package org.singingwizard.genetics.prographs

class Block(val name: String, val operation: Operation) {
  override def toString() = {
    val id = System.identityHashCode(this).toHexString
    s"Block@$id('$name', ${operation})"
  }
  
  def apply[T](port: Port[T]) = {
    require((operation.inputs contains port) || (operation.outputs contains port))
    PortOnBlock(this, port)
  }
  
  def inputs = operation.inputs.map(PortOnBlock(this, _))
  def outputs = operation.outputs.map(PortOnBlock(this, _))
}
case class PortOnBlock[T](block: Block, port: Port[T]) {
  def -->(other: PortOnBlock[T]) = {
    Connection(this, other)
  }
}
case class Connection[T](src: PortOnBlock[T], dst: PortOnBlock[T]) {
  require(src.port.tpe == dst.port.tpe, 
      s"Connections must have the same type on both ends; ports ${src.port} -> ${dst.port}")
}

case class Graph(blocks: Set[Block] = Set(), connections: Set[AnyConnection] = Set()) {
  require(connections forall { c => (blocks contains c.src.block) && (blocks contains c.dst.block) },
      s"Graph contains a connection to a block not in this graph.")   
  
  def +(c: AnyConnection) = Graph(blocks, connections + c)
  def +(b: Block) = Graph(blocks + b, connections)
  
  override def toString() = {
    s"""Graph{
${blocks.mkString("\n")}
=======
${connections.map(c => s"${c.src.block.name}.${c.src.port.name} --> ${c.dst.block.name}.${c.dst.port.name}").mkString("\n")}
}
"""
  }

  def listeningPorts[T](p: PortOnBlock[T]): Set[PortOnBlock[T]] = {
    for(c <- connections if c.src == p) yield {
      c.dst.asInstanceOf[PortOnBlock[T]]
    }
  }
  
  def inputBlocks = blocks.filter(_.operation.isInput)
  def outputBlocks = blocks.filter(_.operation.isOutput)
}