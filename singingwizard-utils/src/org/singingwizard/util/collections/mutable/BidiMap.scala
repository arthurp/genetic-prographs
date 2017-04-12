package org.singingwizard.util.collections.mutable

import collection.mutable

class BidiMap[A, B] { 
  val leftToRight = new mutable.HashMap[A, B]()
  val rightToLeft = new mutable.HashMap[B, A]()
  
  def left = leftToRight.keySet
  def right = rightToLeft.keySet
  
  def +=(p: (A, B)) = {
    leftToRight += p
    rightToLeft += p.swap
  }
  
  def ++=(p: Iterable[(A, B)]) = {
    p.foreach(this += _)
  }
  
  def -=(p: (A, B)) = {
    leftToRight -= p._1
    rightToLeft -= p._2
  }
  
  def --=(p: Iterable[(A, B)]) = {
    p.foreach(this -= _)
  }
}