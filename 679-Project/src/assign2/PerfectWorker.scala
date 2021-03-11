package assign2

import org.apache.log4j.Logger
import parascale.actor.last.{Task, Worker}
import parascale.util._
import parascale.future.perfect._sumOfFactorsInRange

/**
  * Spawns workers on the localhost.
  */
object PerfectWorker extends App {
  val LOG = Logger.getLogger(getClass)

  LOG.info("started")

  // Number of hosts in this configuration
  val nhosts = getPropertyOrElse("nhosts",1)

  // One-port configuration
  val port1 = getPropertyOrElse("port", 8000)

  // If there is just one host, then the ports will include 9000 by default
  // Otherwise, if there are two hosts in this configuration, use just one
  // port which must be specified by VM options
  val ports = if(nhosts == 1) List(port1, 9000) else List(port1)

  // Spawn the worker(s).
  // Note: for initial testing with a single host, "ports" contains two ports.
  // When deploying on two hosts, "ports" will contain one port per host.
  for(port <- ports) {
    // Construction forks a thread which automatically runs the actor act method.
    new PerfectWorker(port)
  }
}

/**
  * Template worker for finding a perfect number.
  * @param port Localhost port this worker listens to
  */
class PerfectWorker(port: Int) extends Worker(port) {
  import PerfectWorker._

  /**
    * Handles actor startup after construction.
    */
  override def act: Unit = {
    val name = getClass.getSimpleName
    LOG.info("started " + name + " (id=" + id + ")")

    // Wait for inbound messages as tasks
    while (true) {
      receive match {
        case task: Task if task.payload.isInstanceOf[Partition] => {

          val start = task.payload.asInstanceOf[Partition].start
          val end = task.payload.asInstanceOf[Partition].end
          val candidate = task.payload.asInstanceOf[Partition].canidate
          val RANGE = 1000000L

          val numPartitions = (end.toDouble / RANGE).ceil.toInt

          // Create the parallel collection and ranges
          val partitions = (0L until numPartitions).par
          val ranges = for (k <- partitions) yield {
            val lower: Long = k * RANGE + start

            val upper: Long = end min (k + 1) * RANGE + start

            (lower, upper)
          }

          // Calculate partial sums for each range
          val t0 = System.nanoTime()
          val sums = ranges.map { lowerUpper =>
            val (lower, upper) = lowerUpper
            _sumOfFactorsInRange(lower, upper, candidate)
          }
          val t1 = System.nanoTime()

          val total = sums.reduce { (a, b) =>
            a + b
          }

          // Send a Result back to the dispatcher
          val result = Result(total, t0, t1)
          sender.send(result)
        }

        case that => {
          // Ignore all other messages
        }
      }
    }
  }
}
