package lab3

/**
  * A simple child thread implemented using Thread
  * @param no child number passed from its parent thread
  */
class ChildThread (no : Int) extends Thread {
  override def run(): Unit = {
    println("child: "+ no + " " + this.getId)
  }

}
