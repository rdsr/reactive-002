package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val v1 = b()
      v1 * v1 - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val d = delta()
      if (d < 0) Set()
      else {
        val bb = b()
        Set((-bb + Math.sqrt(d))/(2 * a()), (-bb - Math.sqrt(d))/(2 * a()))
      }
    }
  }
}
