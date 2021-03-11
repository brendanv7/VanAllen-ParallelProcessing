package assign2

import org.apache.log4j.Logger
import parascale.actor.last.{Dispatcher, Task}
import parascale.util._
import parascale.future.perfect.{_sumOfFactorsInRange, ask, candidates}

/**
  * Spawns a dispatcher to connect to multiple workers.
  */
object PerfectDispatcher extends App {
  val LOG = Logger.getLogger(getClass)
  LOG.info("started")

  // For initial testing on a single host, use this socket.
  // When deploying on multiple hosts, use the VM argument,
  // -Dsocket=<ip address>:9000 which points to the second
  // host.
  val socket2 = getPropertyOrElse("socket","localhost:9000")

  // Construction forks a thread which automatically runs the actor act method.
  new PerfectDispatcher(List("localhost:8000", socket2))
}

/**
  * Template dispatcher which tests readiness of
  * @param sockets
  */
class PerfectDispatcher(sockets: List[String]) extends Dispatcher(sockets) {
  import PerfectDispatcher._

  /**
    * Handles actor startup after construction.
    */
  def act: Unit = {
    LOG.info("sockets to workers = "+sockets)

    (0 until sockets.length).foreach { k =>
      LOG.info("sending message to worker " + k)
      workers(k) ! "to worker(" + k + ") hello from dispatcher"
    }

    val candidate = candidates(8)

    // Create partitions for each worker
    val par1 = Partition(1, candidate/2, candidate)
    val par2 = Partition(candidate/2+1, candidate, candidate)

    // Send tasks to workers
    val t0 = System.nanoTime();
    workers(0) ! par1
    workers(1) ! par2

    // Wait to receive replies
    val results = for(_ <- 1 to workers.length) yield {
      receive match {
        case task: Task if task.kind == Task.REPLY =>
          LOG.info("received reply " + task)
          task.payload match {
            case result: Result =>
              result
          }
      }
    }
    // Both workers have now replied
    val tn = System.nanoTime() - t0

    val sum = results.foldLeft(0L) { (sum: Long, result: Result) =>
      sum + result.sum
    }

    val t1 = results.foldLeft(0L) { (sum: Long, result: Result) =>
      sum + (result.t1 - result.t0)
    }

    val result = if(sum == candidate * 2) "YES" else "NO"

    println(candidate + " is perfect? " + result)
    println("Tn = "+tn/1000000000.0 + "s")
    println("T1 = "+t1/1000000000.0 + "s")
  }
}

