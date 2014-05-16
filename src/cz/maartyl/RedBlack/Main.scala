package cz.maartyl.RedBlack

import scala.util.Random
import scala.annotation.tailrec

object Main {
  var t: BinTree[Int, Int] = _
  val FILE = "/tmp/maallrb.html"

  def main(args: Array[String]): Unit = {

    val rng = 0 to 5 //3917
    val srng = Random shuffle rng zip rng
    t = RBMap(srng: _*)

    spittree

    //println(t)

    repl

  }

  @tailrec def readLineLoop(f: String => Unit): Unit = Option(readLine) match {
    case None => ()
    case Some(row) => { f(row); readLineLoop(f) }
  }

  def repl: Unit = {
    readLineLoop((x) => { interactive(x); spittree })
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def interactive(row: String): Unit = row match {
    case r" *i +(\d+)${ a } +(\d+)${ b } *" => t += (a.toInt -> b.toInt)
    case r" *d +${ a } *" => t -= a.toInt
    case r" *df *" => t = t.tail
    case r" *dl *" => t = t.init
    case r" *c *" => t = RBMap()
    case r" *r +(\d+)${ cnt } *" => {
      val rnd = Random.nextInt / 2 abs
      val rng = rnd to (rnd + cnt.toInt)
      val srng = Random shuffle rng zip rng
      t = RBMap(srng: _*)
    }
    case _ => println("i key val :nsert; d key :elete; df :el irst; dl :ast; c :lear; r count :andom; q :uit")
  }

  def weirdTraverse = {
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

  def spittree = {
    val w = new java.io.PrintWriter(FILE, "UTF-8")
    w write t.htmlDump
    w close
  }

}



