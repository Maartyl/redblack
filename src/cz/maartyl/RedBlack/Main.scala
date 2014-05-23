package cz.maartyl.RedBlack

import scala.util.Random
import scala.annotation.tailrec
import cz.maartyl.Pipe._
import cz.maartyl.Regex._

object Main extends App {
  type BTI = BinTree[Int, Int]
  val stack = scala.collection.mutable.Stack[BTI]()
  val FILE = "/tmp/maallrb.html"
  var vars: BinTree[String, BTI] = RBMap("dflt" -> RBMap[Int, Int]()) // I use my RBMap for dictionary ^^ (no delete required...)

  def push(t: BTI) = stack push t
  def pop: BTI = if (!stack.isEmpty) stack pop else vars("dflt")
  def t = if (stack isEmpty) vars("dflt") else stack head

  def save(name: String, g: BTI) = vars += (name -> g)
  def load(name: String) = vars get name getOrElse vars("dflt")

  private class ExitThrw extends Throwable
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
  def interactive(row: String): Unit = row match {
    case r" *= *([a-zA-Z]+)${ nm }" => save(nm, t)
    case r" *\@ *([a-zA-Z]+)${ nm }" => push(load(nm)) //'at'
    case r" *i *(-?\d+)${ k } +(-?\d+)${ v } *" => push(t + (k.toInt -> v.toInt))
    case r" *i *(-?\d+)${ k } *" => push(t + (k.toInt |> (_ -> _)))
    case r" *d *(-?\d+)${ k } *" => push(t - k.toInt)
    case r" *df *" => push(t tail)
    case r" *dl *" => push(t init)
    case r" *p *" => println(t)
    case r" *c *" => push(RBMap())
    case r" *z *" => pop
    case r" *h *" => printHelp
    case r" *q *" => throw new ExitThrw
    case r" *dbl *" => push(RBMap(t map { case (k, v) => (2 * k, v) } toSeq: _*))
    case r" *seq *([ 0-9-]+)${ s } *" => push(RBMap(s split "[ ]+" map { _ toInt } zip (Stream from 0): _*))
    case r" *r *(\d+)${ cnt } *" => push(RBMap(
      (Random.nextInt(cnt.toInt * 2) / 2 abs) //random start: 0 - cnt
        |> (rnd => rnd to (rnd + cnt.toInt - 1))
        |> { Random shuffle _ zip _ }: _*)) //pair range values randomly and insert unordered
    case s => println("type h for help; %s" format s)
  }

  def printHelp = {
    println("i key val :insert (int int)")
    println("i key     :insert, kye also value (int)")
    println("d key     :delete (int)")
    println("df        :delete first")
    println("dl        :delete last")
    println("dbl       :double keys (for insert between) (doesn't preserve shape)") //TODO: hack
    println("seq k k.. :build tree from keys in given order, values are indices[0..] (int[])")
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

  //#main
  push(RBMap((0 to 5) |> { Random shuffle _ zip _ }: _*))
  spithtml
  //println(t)
  repl
  //#/main

}



