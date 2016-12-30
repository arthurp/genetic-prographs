package org.singingwizard.genetics.prographs

import org.singingwizard.genetics.prographs.interpreter.Interpreter
import java.util.logging.ConsoleHandler
import java.util.logging.Level
import java.util.logging.SimpleFormatter
import java.util.logging.{ Logger ⇒ JLogger }

object Main {
  val consoleHandler = new ConsoleHandler()
  consoleHandler.setLevel(Level.FINEST)
  consoleHandler.setFormatter(new SimpleFormatter())

  val app = JLogger.getLogger("")
  app.setLevel(Level.FINEST)
  app.addHandler(consoleHandler)
  
  def main(args: Array[String]): Unit = {
    import org.singingwizard.genetics.prographs.operations._
    var g = Graph()
    val c1 = Constant(TypeInt, 1)
    val c2 = Constant(TypeInt, 2)
    val v1 = new Block("1", c1)
    val v1_2 = new Block("1_2", c1)
    val v2 = new Block("2", c2)
    val a = new Block("a", Add)
    val b = new Block("b", IntToString)
    val c = new Block("c", Log)
    g = g + a + v1 + v2 + b + c + v1_2
    g += v1(c1.Out) --> a(Add.A)
    g += v2(c2.Out) --> a(Add.B)
    g += v1_2(c1.Out) --> a(Add.B)
    g += a(Add.Sum) --> b(IntToString.In)
    g += b(IntToString.Out) --> c(Log.In)
    println(g)
    val interp = new Interpreter(g)
    interp.run()
  }
}