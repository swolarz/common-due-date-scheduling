package data.util

object DataUtils {

  def autoClose[A <: AutoCloseable](closeable: A)(fun: A ⇒ Unit): Unit = {
    var t: Throwable = null
    try {
      fun(closeable)
    } catch {
      case funT: Throwable ⇒
        t = funT
        throw t
    } finally {
      if (t != null) {
        try {
          closeable.close()
        } catch {
          case closeT: Throwable ⇒
            t.addSuppressed(closeT)
            throw t
        }
      } else {
        closeable.close()
      }
    }
  }
}
