package lab2

object SeqMultAdder extends App{

  val nums = List(1, 3, 4, 5, 12, 2, 7, 9, 7)

  // Part II
  // val f = { n: Int => println(n) }
  // nums.foreach(println(_))
  def f(n: Int): Unit = println(n)

  // Part III
  val odds = nums.filter { n => n % 2 != 0 }
  odds.foreach(println(_))

  // Part IV
  val total = odds.foldLeft(0) { (sum, odd) => sum + 2*odd }
  println(total)

}
