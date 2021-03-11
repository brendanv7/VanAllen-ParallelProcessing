package lab3

/**
  * A simple parent thread which has exactly 1 child thread
  */
object ParentWithThread extends App {
  val numCores = Runtime.getRuntime.availableProcessors
  val child = new Thread(new ChildThread(0))
  child.start()
  val numThreads = Thread.activeCount

  println("parent: "+Thread.currentThread().getId+" "+numCores+" "+numThreads)

  child.join()
}
