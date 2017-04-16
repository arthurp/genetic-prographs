package org.singingwizard.genetics.prographs

import scala.annotation.tailrec
import org.singingwizard.util.collections.RandomSelection._
import org.singingwizard.util.collections.MetricOperations._

case class InterfacedGraph(inputs: Set[PortOnBlock[_]], outputs: Set[PortOnBlock[_]], graph: Graph) {
  def interfaceDifference(o: InterfacedGraph): Double = {
    val Seq(inLen1, inLen2) = Seq(this, o).map(_.inputs.size)
    val Seq(outLen1, outLen2) = Seq(this, o).map(_.outputs.size)
    val Seq(inTypes1, inTypes2) = Seq(this, o).map(_.inputs.map(_.port.tpe))
    val Seq(outTypes1, outTypes2) = Seq(this, o).map(_.outputs.map(_.port.tpe))

    val inDiffs = inTypes1.differenceVector(inTypes2, 1.0)
    val outDiffs = outTypes1.differenceVector(outTypes2, 1.0)

    val diffVec = inDiffs ++ outDiffs
    // This addition to the above expression prefers interfaces with the same sizes.
    val sizeCoords = Seq((inLen1 - inLen2).abs.toDouble / (inLen1 + inLen2), (outLen1 - outLen2).abs.toDouble / (outLen1 + outLen2))

    //println(diffVec)

    (diffVec).pNorm(2)
  }
  
  def interface = inputs ++ outputs
  
  def connect(o: InterfacedGraph): Graph = {
    def connectionsForTpe[T](tpe: Type[T]): Seq[Connection[T]] = {
      def ports(ps: Traversable[PortOnBlock[_]]) : Seq[PortOnBlock[T]] = {
        val ps1 = ps collect {
          case p if p.port.tpe == tpe => p.asInstanceOf[PortOnBlock[T]]
        }
        
        ps1.toSeq.shuffle
      }
      
      val pairings = (ports(this.outputs) zip ports(o.inputs)) ++ (ports(o.outputs) zip ports(this.inputs))
      
      val connections = pairings map { case (out, in) =>
        out --> in
      }
      
      connections
    }
    
    val tpes = (this.interface ++ o.interface).map(_.port.tpe).toSet
    val connections = tpes.flatMap((t: Type[_]) => connectionsForTpe(t): Seq[Connection[_]])
    
    this.graph ++ o.graph addConnections connections
  }
}

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
          val Seq(a, b) = if (s.size == 2) s.toSeq else Seq(s.head, s.head)
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

  def normal(mu: Double, delta: Double)(x: Double) = {
    val x_mu = (x - mu)
    math.exp(-x_mu * x_mu / (2 * delta * delta))
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
  def randomCut(): (InterfacedGraph, Set[AnyConnection], InterfacedGraph) = {
    @tailrec
    def contractToTwo(edgeList: EdgeList): EdgeList = {
      if (edgeList.edges.size == 1)
        edgeList
      else
        contractToTwo(edgeList.contract(edgeList.edges.random()))
    }

    val edges = connections.map(c ⇒ Edge(Set(c.src.block), Set(c), Set(c.dst.block)))

    val edgeList = EdgeList(edges)
    val res = contractToTwo(edgeList)

    assert(res.edges.size == 1)

    val Edge(s1, cs, s2) = res.edges.head

    val g1 = this removeBlocks s2
    val g2 = this removeBlocks s1

    def computeInterface(g: Graph): InterfacedGraph = {
      val ins = cs.map(_.dst).filter(p ⇒ g.blocks contains p.block)
      val outs = cs.map(_.src).filter(p ⇒ g.blocks contains p.block)

      InterfacedGraph(ins, outs, g)
    }

    (computeInterface(g1), cs, computeInterface(g2))
  }

  private val N_TO_GENERATE = 10

  def randomBestInterfacedSubgraph(): InterfacedGraph = {
    val graphs = (0 to N_TO_GENERATE).flatMap { _ ⇒
      val (g1, cs, g2) = this.randomCut()
      Seq(g1, g2)
    }

    val graphSizeCurve = normal(this.blocks.size * 0.25, this.blocks.size * 0.10) _
    val interfaceSizeCurve = normal(this.blocks.map(_.ports.size).pNorm(2) * 1.2 + 1, 3) _

    graphs maxBy { g ⇒
      val interfaceSize = g.inputs.size + g.outputs.size
      val graphSize = g.graph.blocks.size
      graphSizeCurve(graphSize) + interfaceSizeCurve(interfaceSize)
    }
  }

  @tailrec
  final def randomInterfacedSubgraph(n: Int = N_TO_GENERATE*10): (InterfacedGraph, InterfacedGraph) = {
    val idealSize = this.blocks.size * 0.25
    val g = {
      val (g1, _, g2) = this.randomCut()
      (g1, g2)
    }

    def acceptable(gg: InterfacedGraph): Boolean = {
      val interfaceSize = gg.inputs.size + gg.outputs.size
      val graphSize = gg.graph.blocks.size
      interfaceSize >= 2 && graphSize >= 2
    }

    if (n <= 0 || (acceptable(g._1) && acceptable(g._2)))
      g
    else
      randomInterfacedSubgraph(n - 1)
  }

  def randomMatchingSubgraphs(o: Graph) = {
    val Seq(g1Cuts, g2Cuts) = Seq(this, o).map(g ⇒ (0 to N_TO_GENERATE).map { i ⇒
      val gr = g.randomInterfacedSubgraph()
      //      println(s"========= Cut $i (graph size ${gr.graph.blocks.size})")
      //      pprint.pprintln(gr.inputs)
      //      pprint.pprintln(gr.outputs)
      //      pprint.pprintln(gr.graph)
      gr
    })

    val cross = for (a ← g1Cuts ++ g1Cuts.map(_.swap); b ← g2Cuts ++ g2Cuts.map(_.swap)) yield (a, b)
    val ((mA, _), (_, mB)) = cross minBy { case ((x1, x2), (y1, y2)) ⇒
      x1.interfaceDifference(y1)
    }
    
    (mA, mB)
  }
}