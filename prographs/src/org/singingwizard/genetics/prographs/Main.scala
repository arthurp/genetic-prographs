package org.singingwizard.genetics.prographs

import java.util.logging.{ ConsoleHandler, Level, Logger ⇒ JLogger, SimpleFormatter }

import org.singingwizard.genetics.prographs.interpreter.{ IOInput, IOInterpreter, IOOutput }
import org.singingwizard.genetics.prographs.operations._
import org.singingwizard.util.Graphviz

object Main {
  val consoleHandler = new ConsoleHandler()
  consoleHandler.setLevel(Level.FINEST)
  consoleHandler.setFormatter(new SimpleFormatter())

  val app = JLogger.getLogger("")
  app.setLevel(Level.INFO)
  app.addHandler(consoleHandler)

  val Sum = Port("Sum", TypeInt)
  val Max = Port("Max", TypeInt)

  def main(args: Array[String]): Unit = {
    var g = buildSumUpto()
    //println(g)
    //println(g.toDot)
    Graphviz.display(g.toDot)
    val interp = new IOInterpreter(g)
    interp.inputs += (Max -> 10)
    interp.run()
    println(interp.outputs) 
    
    //g align buildSumUpto2()
  }

  def buildSumConstants() = {
    var g = Graph()
    val c1 = Constant(TypeInt, 1)
    val c2 = Constant(TypeInt, 2)
    val v1 = new Block("1", c1)
    val v1_2 = new Block("1_2", c1)
    val v2 = new Block("2", c2)
    val a = new Block("a", Add)
    val b = new Block("b", IntToString)
    val c = new Block("c", Log)
    g += v1(c1.Out) --> a(Add.A)
    g += v2(c2.Out) --> a(Add.B)
    g += v1_2(c1.Out) --> a(Add.B)
    g += a(Add.Sum) --> b(IntToString.In)
    g += b(IntToString.Out) --> c(Log.In)
    g
  }

  def buildSumUpto() = {
    val in = new Block("in", IOInput(Max))
    val v0 = new Block("0", Constant(TypeInt, 0))

    val tvm1 = new Block("-1", TriggeredConstant(TypeInt, -1, TypeInt))
    val tv1 = new Block("1", TriggeredConstant(TypeInt, 1, TypeInt))
    val tv1_2 = new Block("1", TriggeredConstant(TypeInt, 1, TypeInt))
    val add_sum = new Block("sum", Add)
    val add_dec = new Block("dec", Add)
    val switch1 = new Block("c", Switch(TypeInt))
    val switch2 = new Block("c", Switch(TypeInt))
    val lt = new Block("<", LessThan)
    val out = new Block("out", IOOutput(Sum))

    Graph(
      in(in.Out) --> tvm1(tvm1.Trigger),
      switch2(switch2.False) --> tvm1(tvm1.Trigger),

      in(in.Out) --> add_dec(Add.A),
      switch2(switch2.False) --> add_dec(Add.A),
      tvm1(tvm1.Out) --> add_dec(Add.B),

      add_dec(Add.Sum) --> switch2(switch2.In),
      lt(lt.Out) --> switch2(switch2.Pred),

      add_dec(Add.Sum) --> tv1_2(tv1_2.Trigger),

      add_dec(Add.Sum) --> lt(lt.A),
      tv1_2(tv1_2.Out) --> lt(lt.B),

      in(in.Out) --> add_sum(Add.A),
      add_dec(add_dec.Sum) --> add_sum(Add.A),
      v0(v0.Out) --> add_sum(Add.B),
      switch1(switch1.False) --> add_sum(Add.B),

      lt(lt.Out) --> switch1(switch1.Pred),
      add_sum(add_sum.Sum) --> switch1(switch1.In),

      switch1(switch1.True) --> out(out.In)
    )
  }

  def buildSumUpto2() = {
    val in = new Block("in", IOInput(Max))
    val v0 = new Block("0", Constant(TypeInt, 0))

    val tvm1 = new Block("-1", TriggeredConstant(TypeInt, -1, TypeInt))
    val tv1 = new Block("1", TriggeredConstant(TypeInt, 1, TypeInt))
    val tv1_2 = new Block("1", TriggeredConstant(TypeInt, 1, TypeInt))
    val add_sum = new Block("sum", Add)
    val add_dec = new Block("dec", Add)
    val switch1 = new Block("c", Switch(TypeInt))
    val switch2 = new Block("c", Switch(TypeInt))
    val lt = new Block("<", LessThan)
    val out = new Block("out", IOOutput(Sum))

    Graph(
      in(in.Out) --> tvm1(tvm1.Trigger),
      switch2(switch2.True) --> tvm1(tvm1.Trigger),

      in(in.Out) --> add_dec(Add.A),
      switch2(switch2.False) --> add_dec(Add.A),
      tvm1(tvm1.Out) --> add_dec(Add.B),

      add_dec(Add.Sum) --> switch2(switch2.In),
      lt(lt.Out) --> switch2(switch2.Pred),

      add_dec(Add.Sum) --> tv1_2(tv1_2.Trigger),

      add_dec(Add.Sum) --> lt(lt.A),
      tv1_2(tv1_2.Out) --> lt(lt.B),

      in(in.Out) --> add_sum(Add.A),
      add_dec(add_dec.Sum) --> add_sum(Add.A),
      v0(v0.Out) --> add_sum(Add.B),
      switch1(switch1.False) --> add_sum(Add.B),

      lt(lt.Out) --> switch1(switch1.Pred),
      add_sum(add_sum.Sum) --> switch1(switch1.In),

      switch1(switch1.True) --> out(out.In)
    )
  }
}
