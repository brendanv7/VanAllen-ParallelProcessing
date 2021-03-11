import org.apache.log4j.Logger
import parabond.cluster.Partition
import parascale.actor.last.{Task, Worker}
import parascale.future.perfect._sumOfFactorsInRange
import parascale.parabond.util.{Job, Result}
import parascale.util._

object ParaWorker extends App {
  val LOG = Logger.getLogger(getClass)
  //a. If worker running on a single host, spawn two workers
  // else spawn one worker.
  val nhosts = getPropertyOrElse("nhosts", 1)

  //Set the node, default to basic node
  val prop = getPropertyOrElse("node","parabond.cluster.BasicNode")

  val clazz = Class.forName(prop)

  import parabond.cluster.Node
  val node = clazz.newInstance.asInstanceOf[Node]

  // One-port configuration
  val port1 = getPropertyOrElse("port", 8000)

  // If there is 1 host, then ports include 9000 by default
  // Otherwise, if there are two hosts in this configuration,
  // use just one port which must be specified by VM options
  val ports = if (nhosts == 1) List(port1, 9000) else List(port1)

  // Spawn the worker(s).
  // Note: for initial testing with a single host, "ports"
  // contains two ports. When deploying on two hosts, "ports"
  // will contain one port per host.

  for (port <- ports) {
    // Start up new worker.
    new ParaWorker(port)
  }
}

class ParaWorker(port: Int) extends Worker(port) {
  import ParaWorker._

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

          val partition = task.payload.asInstanceOf[Partition]

          val analysis = node analyze(partition)

          val partialT1 = analysis.results.foldLeft(0L) { (sum: Long, job: Job) =>
            sum + (job.result.t1 - job.result.t0)
          }

          sender ! Result(partialT1)
        }

        case that => {
          // Ignore all other messages
        }
      }
    }
  }
}