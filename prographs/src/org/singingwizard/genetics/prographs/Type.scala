package org.singingwizard.genetics.prographs

import scala.reflect.ClassTag
import java.lang.{Byte, Short, Long, Boolean, Character, Float, Double}
  

sealed abstract class Type[_T : ClassTag] {
  type T = _T
  
  def isInstance(v: Any) = {
    val cls = implicitly[ClassTag[T]].runtimeClass match {
      case Integer.TYPE => classOf[Integer]
      case Byte.TYPE => classOf[Byte]
      case Short.TYPE => classOf[Short]
      case Boolean.TYPE => classOf[Boolean]
      case Character.TYPE => classOf[Character]
      case Float.TYPE => classOf[Float]
      case Double.TYPE => classOf[Double]
      case c => c
    }
    //Logger.finer(s"Checking type of $v against $this (${implicitly[ClassTag[T]].runtimeClass}, $cls)")
    cls.isInstance(v)
  }
}

case object TypeInt extends Type[Int]
case object TypeString extends Type[String]
