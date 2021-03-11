package lab3

/**
  * A simple child thread implemented using Runnable
  * @param no child number passed from its parent thread
  */
class ChildRunnable (no : Int) extends Runnable{
  override def run(): Unit = {
    println("child: "+ no + " " + Thread.currentThread().getId)
  }
}
