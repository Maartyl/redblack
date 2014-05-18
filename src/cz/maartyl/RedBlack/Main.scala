package cz.maartyl.RedBlack

import scala.util.Random
import scala.annotation.tailrec

object Main {
  type BTI = BinTree[Int, Int]
  val stack = scala.collection.mutable.Stack[BTI]()
  val FILE = "/tmp/maallrb.html"

  def push(t: BTI) = stack push t
  def pop: BTI = if (!stack.isEmpty) stack pop else vars("dflt")
  def t = if (stack isEmpty) vars("dflt") else stack head

  var vars: BinTree[String, BTI] = RBMap("dflt" -> RBMap[Int, Int]()) // I use my RBMap for dictionary ^^

  def save(name: String, g: BTI) = {
    vars += (name -> g)
  }
  def load(name: String) = {
    vars get name match {
      case None => vars("dflt")
      case Some(g) => g
    }
  }

  def main(args: Array[String]): Unit = {

    val rng = 0 to 5 //3917
    val srng = Random shuffle rng zip rng
    push(RBMap(srng: _*))

    spithtml

    //println(t)

    repl

  }

  @tailrec private def repl: Unit =
    try for (line <- io.Source.stdin.getLines) { interactive(line); spithtml }
    catch {
      case e: ExitThrw => println("Exit")
      case e: Exception => {
        println(e getMessage)

        print("Print StackTrace? (y/n/q): ")
        if (readLine match {
          case r"[qQ]" => println("Exit") == e
          case r"[yY]" => { e printStackTrace; true }
          case _ => true
        }) repl else ()
      }
    }

  private class ExitThrw extends Throwable

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail map { _ => "x" }: _*)
  }

  def interactive(row: String): Unit = row match {
    case r" *= *([a-zA-Z]+)${ nm }" => save(nm, t)
    case r" *\@ *([a-zA-Z]+)${ nm }" => push(load(nm)) //'at'
    case r" *i *(-?\d+)${ a } +(-?\d+)${ b } *" => push(t + (a.toInt -> b.toInt))
    case r" *d *(-?\d+)${ a } *" => push(t - a.toInt)
    case r" *df *" => push(t tail)
    case r" *dl *" => push(t init)
    case r" *p *" => println(t)
    case r" *c *" => push(RBMap())
    case r" *z *" => pop
    case r" *h *" => printHelp
    case r" *q *" => throw new ExitThrw
    case r" *r *(\d+)${ cnt } *" => {
      val rnd = Random.nextInt(cnt.toInt * 2) / 2 abs
      val rng = rnd to (rnd + cnt.toInt - 1)
      val srng = Random shuffle rng zip rng
      push(RBMap(srng: _*))
    }
    case s => println("type h for help; %s" format s)
  }

  def printHelp = {
    println("i key val :insert (int int)")
    println("d key     :delete (int)")
    println("df        :delete first")
    println("dl        :delete last")
    println("z         :ctrl-z")
    println("p         :print")
    println("c         :clear")
    println("= name    :save current map under `name` (string)")
    println("@ name    :load map perviously saved under `name` (string)")
    println("r size    :build random tree (int)")
    println("h         :this help")
    println("q         :quit")
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

  def spithtml = {
    val w = new java.io.PrintWriter(FILE, "UTF-8")
    w write t.htmlDump
    w close
  }

}



