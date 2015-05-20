package calculator

object Polynomial {
  def computeDelta(
      a: Signal[Double]
    , b: Signal[Double]
    , c: Signal[Double]
    ): Signal[Double] = {
    Signal{b() * b() - 4 * a() * c()}
  }

  def computeSolutions(
      a: Signal[Double]
    , b: Signal[Double]
    , c: Signal[Double]
    , delta: Signal[Double]
    ): Signal[Set[Double]] = {
    Signal{
//      val delta = computeDelta(a, b, c)
      if(delta() < 0) {
        Set.empty
      } else {
        val left  = (-b() + scala.math.sqrt(delta())) / (2 * a())
        val right = (-b() - scala.math.sqrt(delta())) / (2 * a())
        Set(left, right)
      }
    }
  }
}
