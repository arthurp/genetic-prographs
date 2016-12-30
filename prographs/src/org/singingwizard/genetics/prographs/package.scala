package org.singingwizard.genetics

import org.singingwizard.util.ObjectLogger

package object prographs extends ObjectLogger {
  type Value = AnyRef
  type AnyPort = Port[_]
  type AnyConnection = Connection[_]
}