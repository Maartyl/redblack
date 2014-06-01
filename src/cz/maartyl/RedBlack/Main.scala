package cz.maartyl.RedBlack

import scala.util.Random
import scala.annotation.tailrec
import cz.maartyl.Pipe._ //impl: |>
import cz.maartyl.Regex._ //impl: r"regex${a}"

/**
 * Simple app to test RBMap
 * Works as REPL that regenerates file representing current tree structure
 * REPL allows modifing current RBMap, rebuilding it; returning back in history; saving current RBMaps under names and more
 *
 * If 0 arguments are given, default file location, in system temporary folder with 'maallrb.html' name, is chosen
 * If 1 argument is given, it is used for file location
 *
 * @author  Maartyl
 */

object Main extends App {
  type BTI = BinTree[Int, Int]
  val stack = scala.collection.mutable.Stack[BTI]()

  val FILE = if (args.length > 0) args(0)
  else util.Properties.tmpDir + (if (util.Properties isWin) "\\" else "/") + "maallrb.html"

  var vars: BinTree[String, BTI] = RBMap("dflt" -> RBMap[Int, Int]()) // I use my RBMap for dictionary ^^ (no delete required...)

  def push(t: BTI) = stack push t //new BTI state to be displayed; pushes to undo history
  def pop: BTI = if (!stack.isEmpty) stack pop else vars("dflt") //undo
  def t = if (stack isEmpty) vars("dflt") else stack head //just history first: current BTI

  def save(name: String, g: BTI) = vars += (name -> g) //binds given BTI to given name
  def load(name: String) = vars get name getOrElse vars("dflt") //returns BTI associated with given name

  private class ExitThrw extends Throwable //thrown from `interactive` to exit REPL
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
    case r" *seq *([ 0-9-]+)${ s } *" => push(RBMap(s split " +" map { _ toInt } zip (Stream from 0): _*))
    case r" *r *(\d+)${ cnt } *" => push(RBMap(
      (Random.nextInt(cnt.toInt * 2) / 2 abs) //random start: 0 - cnt
        |> (rnd => rnd to (rnd + cnt.toInt - 1))
        |> { Random shuffle _ zip _ }: _*)) //pair range values randomly and insert unordered
    case s if s.length > 0 => println("type h for help; %s" format s)
    case _ => () //pass : empty line
  }

  def printHelp = {
    println("i key val :insert (int int)")
    println("i key     :insert, key also used as value (int)")
    println("d key     :delete (int)")
    println("df        :delete first")
    println("dl        :delete last")
    println("dbl       :double keys (for insert between) (doesn't preserve shape)") //TODO: hack shape
    println("seq k k.. :build tree from keys in given order, values are indices[0..] (int[])")
    println("z         :ctrl-z; undo")
    println("p         :print")
    println("c         :clear")
    println("= name    :save current map under `name` (string)")
    println("@ name    :load map perviously saved under `name` (string)")
    println("r size    :build random tree (int)")
    println("h         :this help")
    println("q         :quit")
  }

  def spithtml = { //(re)generates file representing current BTI 
    val w = new java.io.PrintWriter(FILE, "UTF-8")
    w write t.htmlDump
    w close
  }

  //#main
  push(RBMap((0 to 5) |> { Random shuffle _ zip _ }: _*)) //initialize with some simple BTI for start
  spithtml
  //println(t)
  println("Started, using file: " + FILE)
  repl
  //possibly delete file... (actually, for testing purposes, it is better if file is left there even after app ends)
  //#/main
}



