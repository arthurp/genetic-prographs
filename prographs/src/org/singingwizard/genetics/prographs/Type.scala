package org.singingwizard.genetics.prographs

import scala.reflect.ClassTag

sealed abstract class Type[_T: ClassTag] {
  type T = _T

  def isInstance(v: Any) = {
    val cls = implicitly[ClassTag[T]].runtimeClass match {
      case java.lang.Integer.TYPE   ⇒ classOf[java.lang.Integer]
      case java.lang.Byte.TYPE      ⇒ classOf[java.lang.Byte]
      case java.lang.Short.TYPE     ⇒ classOf[java.lang.Short]
      case java.lang.Boolean.TYPE   ⇒ classOf[java.lang.Boolean]
      case java.lang.Character.TYPE ⇒ classOf[java.lang.Character]
      case java.lang.Float.TYPE     ⇒ classOf[java.lang.Float]
      case java.lang.Double.TYPE    ⇒ classOf[java.lang.Double]
      case c                        ⇒ c
    }
    //Logger.finer(s"Checking type of $v against $this (${implicitly[ClassTag[T]].runtimeClass}, $cls)")
    cls.isInstance(v)
  }
}

case object TypeInt extends Type[Int]
case object TypeString extends Type[String]
case object TypeBoolean extends Type[Boolean]
