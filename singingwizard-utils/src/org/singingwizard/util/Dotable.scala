package org.singingwizard.util

trait Dotable {
  def dotName: String
}

trait DotableGraph extends Dotable {
  def toDot(): String
}

trait DotableEdge extends Dotable {
  def toDot(): String
}

trait DotableNode extends Dotable {
  def toDot(): String
}