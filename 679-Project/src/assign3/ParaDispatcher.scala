package assign3

import org.apache.log4j.Logger
import parascale.actor.last.{Dispatcher, Task}
import parascale.parabond.casa.MongoHelper
import parascale.util._
import parascale.parabond.util.Result
import parabond.cluster._
import parascale.parabond.util.Constant.DIAGS_DIR

object ParaDispatcher extends App {
  val LOG = Logger.getLogger(getClass)
  // For initial testing on a single host, use this socket.
  // When deploying on multiple hosts, use the VM argument,
  // -Dsocket=<ip address>:9000 which points to the second
  // host.
  val socket2 = getPropertyOrElse("socket","localhost:9000")
  // This spawns a list of relay workers at the sockets
  new ParaDispatcher(List("localhost:8000", socket2))
}

/**
  * Template dispatcher which tests readiness of
  * @param sockets
  */
class ParaDispatcher(sockets: List[String]) extends Dispatcher(sockets) {
  import ParaDispatcher._

  /**
    * Handles actor startup after construction.
    */
  def act: Unit = {
    val me =  this.getClass().getSimpleName()

    val dir = getPropertyOrElse("dir",DIAGS_DIR)

    val outFile = dir + me + "-dat.txt"

    val fos = new java.io.FileOutputStream(outFile,true)
    val os = new java.io.PrintStream(fos)

    val numCores = 12 // 4 on each host

    os.println("ParaBond Analysis\n" +
      "by Brendan Van Allen\n" +
      "24 Apr 2019\n" +
      "CoarseGrainedNode\n" +
      "Workers: " + sockets.size + "\n" +
      "Hosts: " + workers(0).forwardAddr + " (dispatcher), " + workers(0).forwardAddr + " (worker), " +
      workers(1).forwardAddr + " (worker), " +  MongoHelper.getHost + " (mongo)\n" +
      "Cores: " + numCores + "\n" +
      "N\tmissed\tT1\t\tTN\t\tR\t\t\te\t")

    val ramp = List(
      1000,
      2000,
      4000,
      8000,
      16000,
      32000,
      64000,
      100000
    )


    ramp.foreach { n =>
      val portfIds = checkReset(n, 0)

      val par1 = Partition(n/2, 0)
      val par2 = Partition(n/2, n/2)

      val t0 = System.nanoTime()
      workers(0) ! par1
      workers(1) ! par2

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
      // All workers have replied
      val tn = System.nanoTime() - t0;

      val t1 = results.foldLeft(0L) { (sum, result) =>
        sum + (result.t1 - result.t0)
      }

      val missedPortfs = check(portfIds)

      val r = t1.toDouble / tn.toDouble

      val e = r / numCores

      os.println(n + "\t" + missedPortfs.size + "\t" + (t1 seconds) + "\t" + (tn seconds) + "\t" + r + "\t" + e + "\t")
    }
  }
}
