package cz.maartyl.RedBlack

sealed abstract class RBNode[+K, +TVal] extends BinTreeNode[K, TVal] {
  //[ A >: K,B >:TVal]
  def isNil: Boolean = false

  //#members
  def key: K
  def value: TVal
  def left: RBNode[K, TVal]
  def right: RBNode[K, TVal]
  def clr: RBClr

  //#node methods
  def red: Boolean = clr ? Red
  def black: Boolean = clr ? Black

  def asRed = if (red) this else RBN(left, Red, key, value, right)
  def asBlack = if (black) this else RBN(left, Black, key, value, right)
  def asOpposite: RBNode[K, TVal] = RBN(left, !clr, key, value, right)
  def hasLeft = left != RBNil
  def hasRight = right != RBNil
  def pair = (key, value)

  def arity = if (black) //type of (2-3-4) node
    if (left.red)
      if (right.red) 4
      else 3
    else 2
  else 1 //red

  def condFlip() = {
    if (left.red && right.red)
      RBN(left.asOpposite, !clr, key, value, right.asOpposite)
    else this
  }

  def rotateRight[TK >: K, TV >: TVal]: RBNode[TK, TV] = this match {
    case RBN(RBN(l, c1, k1, v1, m), c2, k2, v2, r) => RBN(l, c1, k1, v1, RBN(m, c2, k2, v2, r))
    case _ => throw new Exception("Invalid: rotateRight")
  }
  def rotateLeft[TK >: K, TV >: TVal]: RBNode[TK, TV] = this match {
    case RBN(l, c1, k1, v1, RBN(m, c2, k2, v2, r)) => RBN(RBN(l, c1, k1, v1, m), c2, k2, v2, r)
    case _ => throw new Exception("Invalid: rotateLeft")
  }

  //haskell.LLRB inspired balancing
  def balanceRight[TK >: K, TV >: TVal](nr: RBNode[TK, TV]) = //arg: new right
    if (black && left.red && nr.red) //slurp (split "4-node") (allowing 4-nodes didn't work...)
      RBN(left.asBlack, Red, key, value, nr.asBlack)
    else if (nr.red) //l.B-> rotate left, switch colors
      RBN(RBN(left, Red, key, value, nr.left), clr, nr.key, nr.value, nr.right)
    else
      RBN(left, clr, key, value, nr)

  def balanceLeft[TK >: K, TV >: TVal](nl: RBNode[TK, TV]) = //arg: new left
    if (black && nl.red && nl.left.red) //split line and rotate
      RBN(nl.left.asBlack, Red, nl.key, nl.value, RBN(nl.right, Black, key, value, right))
    else
      RBN(nl, clr, key, value, right)

  override def toString() = "(%s [%s %s: %s] %s)".format(left, clr, key, value, right)

  def htmlDump: scala.xml.Elem =
    <table class="tg">
      <tr>
        <th colspan="2"><div class={ clr.toString.toLowerCase }>{ key } → { value }</div></th>
      </tr>
      <tr>
        <td>{ left.htmlDump }</td>
        <td>{ right.htmlDump }</td>
      </tr>
    </table>
}

object RBNode {
  def unapply[K, B](n: RBNode[K, B]) = if (n.isNil) None else Some((n.key, n.clr, n.value, n.left, n.right))

  //check nil - possibly avoid elsewhere
  def onNode[T, K, B](child: RBNode[K, B], dflt: T = ())(f: RBNode[K, B] => T) =
    child match {
      case RBNil => dflt
      case v => f(v)
    }

  def mkLeaf[K, V](k: K, v: V) = RBN(RBNil, Red, k, v, RBNil)
  def mkBlackLeaf[K, V](k: K, v: V) = RBN(RBNil, Black, k, v, RBNil)
}

case class RBN[K, V](left: RBNode[K, V], clr: RBClr, key: K, value: V, right: RBNode[K, V]) extends RBNode[K, V]

case object RBNil extends RBNode[Nothing, Nothing] {
  override def clr = Black
  override def key: Nothing = throw new NoSuchElementException("RBNil.key")
  override def value: Nothing = throw new NoSuchElementException("RBNil.value")
  override def isNil = true
  override def left = RBNil
  override def right = RBNil

  override def toString() = "()"
  override def htmlDump = <div class="nil"></div>
}

trait RBClr {
  def ?(o: RBClr) = this equals o
  def unary_! = if (this ? Red) Black else Red
}
case object Red extends RBClr
case object Black extends RBClr










