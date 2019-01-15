package data.loader

import java.io.InputStream
import java.util.Scanner

import data.SchedulingInstance
import data.util.InstancesInfo._

class UpperBoundsLoader {
  private val hCmpEps = 0.00001

  private val upperBounds = Array.ofDim[Int](nValues.length, kValues.length, hValues.length)
  private val loaded = Array.ofDim[Boolean](nValues.length)

  def getUpperBound(instance: SchedulingInstance): Int = getUpperBound(instance.jobs.length, instance.k, instance.h)

  def getUpperBound(n: Int, k: Int, h: Double): Int = {
    val nIx = nValues.indexOf(n)
    val kIx = kValues.indexOf(k)
    val hIx = hValues.indexWhere(hv => (h - hv).abs < hCmpEps)

    if (nIx < 0 || kIx < 0 || hIx < 0)
      throw new IllegalArgumentException(s"Check your instance parameters (n = $n / k = $k / h = $h).")

    if (!loaded(nIx))
      loadUpperBounds(nIx)

    upperBounds(nIx)(kIx)(hIx)
  }

  private def loadUpperBounds(nIx: Int): Unit = {
    val n: Int = nValues(nIx)
    val filepath = s"/upperbound/ubsch$n.txt"

    val inputStream: InputStream = getClass.getResourceAsStream(filepath)
    val scanner: Scanner = new Scanner(inputStream)

    kValues.indices foreach { kIx =>
      hValues.indices foreach { hIx =>
        upperBounds(nIx)(kIx)(hIx) = scanner.nextInt
      }
    }

    loaded(nIx) = true
  }
}
