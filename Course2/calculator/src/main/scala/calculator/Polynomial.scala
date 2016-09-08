package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var {
      val discriminant = delta()
      val denominator: Double = 2 * a()
      val numerator: Double = -1 * b()

      val solutions : Set[Double] = Set()
      if (discriminant < 0) solutions
      else if (discriminant == 0) solutions + (numerator / denominator)
      else {
        val rootDelta: Double = math.sqrt(discriminant)
        solutions ++ Set((numerator + rootDelta) / denominator, (numerator - rootDelta) / denominator)
      }
    }
  }
}
