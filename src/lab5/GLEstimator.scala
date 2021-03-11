package lab5

import scala.math.pow

/**
  * Estimates pi using the Gregory-Leibniz series
  */
object GLEstimator extends App{

  val startTime = System.nanoTime() // t0
  //val pi = (0 until Int.MaxValue).foldLeft(0.0) { (estPi, i) => estPi + (pow(-1,(if ((i & 1) == 1) -1 else 1)) / (2.0 * i + 1))  } * 4.0
  val pi = _piInRange(0, Int.MaxValue) * 4
  val runTime = (System.nanoTime() - startTime) / 1000000000.0 // (t1 - t0) reduced to seconds

  println("Pi = " + pi)
  println(f"dt = $runTime%1.2f")

  /*
    * Using this form to ensure that we don't lose precision
    * when the numbers get very large
    */
  def _piInRange(lower: Long, upper: Long): Double = {
    var index = lower
    var pi = 0.0
    while(index < upper) {
      val numerator = if ((index & 1) == 1) -1 else 1
      pi += numerator / (2.0 * index + 1)
      index += 1
    }
    pi
  }
}
