package cz.maartyl.RedBlack

import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = {

    val rng = 0 to 3917
    val srng = Random shuffle rng zip rng
    val t = RBMap(srng: _*)

    val w = new java.io.PrintWriter("/tmp/maallrb.html", "UTF-8")
    w write t.htmlDump
    w close

    println(t)
//    def pps(x: Any) = { print("[%s]" format x); x toString }
//
//    val (a, b, c) = t.traverse(pps, pps, pps, (n) => { print("{%s}" format(n value)); n key })
//
//    println("---")
//    for (q <- a) print("(%s)" format q)
//    println("---")
//    for (q <- b) print("(%s)" format q)
//    println("---")
//    for (q <- c) print("(%s)" format q)
//    println("---")

  }

}



