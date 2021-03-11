package lab3

/**
  * A simple parent thread which has the same number of
  * children as there are cores.
  */
object ParentWithRunnable extends App {
  val numCores = Runtime.getRuntime.availableProcessors

  val children = for(no <- 0 until numCores) yield {
    val child = new Thread(new ChildRunnable(no))
    child.start
    child
  }

  val numThreads = Thread.activeCount

  println("parent: "+Thread.currentThread().getId+" "+numCores+" "+numThreads)

  for(c <- children) {
    c.join()
  }
}
