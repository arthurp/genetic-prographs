package org.singingwizard.util

/** A bunch of functions for generate graphs from various parts of the system.
  */
object Graphviz {
  /** Take a dot string and render it using "dot" and display the result using "eog". This will block until "eog" is closed.
    */
  def display(dot: String) {
    render(dot, "/tmp/tmp.png")

    val eogProc = Runtime.getRuntime.exec("exo-open /tmp/tmp.png")
    eogProc.waitFor()
  }

  /** copy all data in `is` into `os`.
    */
  private def copyStream(is: java.io.InputStream, os: java.io.OutputStream) {
    val buf = new Array[Byte](512)
    var n = 0
    n = is.read(buf)
    while (n > 0) {
      os.write(buf, 0, n)
      n = is.read(buf)
    }
  }

  /** Render the dot string into a PNG file using "dot".
    */
  def render(dot: String, fn: String) {
    val dotProc = Runtime.getRuntime.exec("dot /dev/stdin -Tpng -o " + fn + "")
    dotProc.getOutputStream.write(dot.getBytes)
    dotProc.getOutputStream.close()
    copyStream(dotProc.getErrorStream, System.err)
    copyStream(dotProc.getInputStream, System.out)
    dotProc.waitFor()
  }
}