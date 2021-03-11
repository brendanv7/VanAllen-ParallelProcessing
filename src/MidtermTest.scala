object MidtermTest extends App {
  val lista = List(5, 12, 1, 99, 14)

  if (lista.isInstanceOf[List[Int]]) {
    val listb = for (x <- lista if x%2 == 1) yield x

    val listc = listb.map{n => n*3}

    println(listb)
    println(listc)
  }

  //println(size)

  println(lista)
}
