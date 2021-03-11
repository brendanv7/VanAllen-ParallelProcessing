package lab6

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.math.pow

/**
  * Estimates pi using the Gregory-Leibniz series
  */
object FuturesGLEstimator extends App{

  val startTime = System.nanoTime() // t0
  val pi = estimatePi()
  val runTime = (System.nanoTime() - startTime) / 1000000000.0 // (t1 - t0) reduced to seconds

  val numCores = Runtime.getRuntime.availableProcessors() / 2 // divide by 2 to get number of physical cores

  println("Pi = " + pi)
  println("N = " + numCores)
  println(f"dt = $runTime%1.2f")

  def piInRange(lower: Long, upper: Long): Double = {
    (lower to upper).foldLeft(0.0) { (estPi, i) => estPi + (pow(-1,i) / (2.0 * i + 1))  }
  }

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

  def estimatePi(): Double = {
    val RANGE = 1000000L

    val numPartitions = (Int.MaxValue.toDouble / RANGE).ceil.toInt;

    val futures = for(k <- 0L until numPartitions) yield Future {
      val lower: Long = k*RANGE
      val upper: Long = Int.MaxValue.toLong min (k + 1) * RANGE

      _piInRange(lower,upper)
    }

    val total = futures.foldLeft(0.0) { (sum, future) =>
      import scala.concurrent.duration._
      val result = Await.result(future, Duration.Inf)

      sum + result
    }

    total * 4.0
  }
}
