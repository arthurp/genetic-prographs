package org.singingwizard.genetics.prographs


object Main {
  def main(args: Array[String]): Unit = {
    import org.singingwizard.genetics.prographs.operations._
    var g = Graph()
    val c1 = Constant(TypeInt, 1)
    val c2 = Constant(TypeInt, 2)
    val v1 = new Block("1", c1)
    val v2 = new Block("2", c2)
    val a = new Block("a", Add)
    val b = new Block("b", IntToString)
    g = g + a + v1 + v2 + b
    g += v1(c1.Out) --> a(Add.A)
    g += v2(c2.Out) --> a(Add.B)
    g += a(Add.Sum) --> b(IntToString.In)
    println(g)
  }
}