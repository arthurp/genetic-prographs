package org.singingwizard.genetics.prographs

import scala.annotation.tailrec
import org.singingwizard.util.collections.RandomSelection._

case class InterfacedGraph(inputs: PortOnBlock[_], outputs: PortOnBlock[_], graph: Graph)

object GraphCutting {
  type Node = Set[AnyBlock]
  case class Edge(a: Node, connections: Set[AnyConnection], b: Node) {
    override def hashCode() = a.hashCode() + b.hashCode() + (connections.hashCode() * 37)
    override def equals(o: Any) = o match {
      case Edge(`a`, `connections`, `b`) ⇒ true
      case Edge(`b`, `connections`, `a`) ⇒ true
      case _                             ⇒ false
    }

    override def toString() = s"Edge($a, $b)"

    def isIncident(n: Node) = a == n || b == n
  }

  object EdgeList extends ((Set[Edge]) ⇒ EdgeList) {
    def apply(edges: Set[Edge]): EdgeList = {
      val edges1 = edges.groupBy(e ⇒ Set(e.a, e.b)) map {
        case (s, es) ⇒
          val Seq(a, b) = s.toSeq
          //pprint.pprintln((a, b, es.size, es))
          Edge(a, es.flatMap(_.connections), b)
      }
      new EdgeList(edges1.toSet)
    }

    def unapply(e: EdgeList): Option[Set[Edge]] = Some(e.edges)

    override def toString =
      getClass.getName.split("""\$""").reverse.dropWhile(x ⇒ { val char = x.take(1).head; !((char == '_') || char.isLetter) }).head
  }

  class EdgeList private (val edges: Set[Edge]) {
    assert {
      edges.forall(x ⇒ edges.forall({ y ⇒
        val b = x.copy(connections = Set()) != y.copy(connections = Set()) || x == y
        assert(b, s"$x and $y have the same end points.")
        b
      }))
    }

    def incidentEdges(n: Node) = edges.filter(_.isIncident(n))

    def contract(e: Edge) = {
      val a = e.a
      val b = e.b
      val n = a ++ b

      EdgeList(edges flatMap {
        case `e` ⇒ None
        case Edge(`a` | `b`, cons, b1) ⇒
          Some(Edge(n, cons, b1))
        case Edge(a1, cons, `a` | `b`) ⇒
          Some(Edge(a1, cons, n))
        case e ⇒ Some(e)
      })
    }

    override def toString() = edges.mkString("EdgeList(", ", ", ")")
    override def hashCode() = edges.hashCode()
    override def equals(o: Any) = o match {
      case EdgeList(`edges`) ⇒ true
      case _                 ⇒ false
    }
  }
}

trait GraphCutting {
  this: Graph ⇒

  import GraphCutting._

  /** Cut this graph into two graphs with specified interfaces.
    *
    * An interface is a set of input ports and a set of output ports.
    *
    * The cut is selected randomly.
    */
  def randomCut(): (InterfacedGraph, InterfacedGraph) = {
    @tailrec
    def contractToTwo(edgeList: EdgeList): EdgeList = {
      if (edgeList.edges.size == 1)
        edgeList
      else
        contractToTwo(edgeList.contract(edgeList.edges.random()))
    }

    val edges = connections.map(c ⇒ Edge(Set(c.src.block), Set(c), Set(c.dst.block)))

    for (i ← 0 to 10) {
      val edgeList = EdgeList(edges)
      val res = contractToTwo(edgeList)

      println("Contraction " + i)
      
      pprint.pprintln(res.edges.head.a)
      pprint.pprintln(res.edges.head.connections)
      pprint.pprintln(res.edges.head.b)
    }

    ???
  }
}