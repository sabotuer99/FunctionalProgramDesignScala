package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      //  b² - 4ac
      val _a = a.apply()
      val _b = b.apply()
      val _c = c.apply()
      _b * _b - 4 * _a * _c
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / 2a
    Signal {
      delta.apply() match {
        case i if i < 0 => Set()
        case i if i >= 0 => {
          val _a = a.apply()
          val _b = b.apply()
          val first = (_b + i) / (2 * _a)
          val second = (_b - i) / (2 * _a)
          Set(first, second)
        }
      }
    }
  }
}
