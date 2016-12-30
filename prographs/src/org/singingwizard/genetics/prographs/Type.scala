package org.singingwizard.genetics.prographs

import scala.reflect.ClassTag

sealed abstract class Type[_T : ClassTag] {
  type T = _T
  
  def isInstance(v: Any) = 
    implicitly[ClassTag[T]].runtimeClass.isInstance(v)
}

case object TypeInt extends Type[Int]
case object TypeString extends Type[String]
