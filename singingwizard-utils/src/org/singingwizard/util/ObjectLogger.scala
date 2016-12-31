package org.singingwizard.util

import java.util.logging.{ Logger ⇒ JLogger }

trait ObjectLogger {
  val Logger = JLogger.getLogger(this.getClass().getPackage().getName())
}