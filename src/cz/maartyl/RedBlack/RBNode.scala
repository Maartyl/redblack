package cz.maartyl.RedBlack

sealed abstract class RBNode[+K, +TVal] extends BinTreeNode[K, TVal] {
  def isNil: Boolean = false

  //#members
  def key: K
  def value: TVal
  def left: RBNode[K, TVal]
  def right: RBNode[K, TVal]
  def clr: RBColor

  //#node methods
  def red: Boolean = clr ? Red
  def black: Boolean = clr ? Black

  def asRed = if (red) this else copy(c = Red)
  def asBlack = if (black) this else copy(c = Black)
  def asOpposite: RBNode[K, TVal] = copy(c = !clr)
  def hasLeft = left != RBNil
  def hasRight = right != RBNil
  def pair = (key, value)

  def isRedLeaf = !(black || hasLeft || hasRight)
  def blb = black && left.black
  def blr = black && left.red

  def copy[TK >: K, TV >: TVal](l: RBNode[TK, TV] = left, c: RBColor = clr,
                                k: TK = key, v: TV = value,
                                r: RBNode[TK, TV] = right) = RBN(l, c, k, v, r)

  def rotateRight[TK >: K, TV >: TVal]: RBNode[TK, TV] = this match {
    case RBN(RBN(l, c1, k1, v1, m), c2, k2, v2, r) => RBN(l, c1, k1, v1, RBN(m, c2, k2, v2, r))
    case _ => throw new Exception("Invalid: rotateRight")
  }
  def rotateLeft[TK >: K, TV >: TVal]: RBNode[TK, TV] = this match {
    case RBN(l, c1, k1, v1, RBN(m, c2, k2, v2, r)) => RBN(RBN(l, c1, k1, v1, m), c2, k2, v2, r)
    case _ => throw new Exception("Invalid: rotateLeft")
  }

  def balanceRight[TK >: K, TV >: TVal](nr: RBNode[TK, TV], // new right
                                        l: RBNode[TK, TV] = left,
                                        c: RBColor = clr,
                                        k: TK = key,
                                        v: TV = value) =
    if (black && l.red && nr.red) //slurp (split "4-node")
      RBN(l asBlack, Red, k, v, nr asBlack)
    else if (nr.red) //l.B-> rotate left, switch colors
      RBN(RBN(l, Red, k, v, nr left), c, nr key, nr value, nr right)
    else
      RBN(l, c, k, v, nr)

  def balanceLeft[TK >: K, TV >: TVal](nl: RBNode[TK, TV], // new left
                                       r: RBNode[TK, TV] = right,
                                       c: RBColor = clr,
                                       k: TK = key,
                                       v: TV = value) =
    if (black && nl.red && nl.left.red) //split line and rotate
      RBN(nl.left.asBlack, Red, nl key, nl value, RBN(nl right, Black, k, v, r))
    else
      RBN(nl, c, k, v, r)

  def balance[TK >: K, TV >: TVal](l: RBNode[TK, TV] = left, //just test, didn't help, not gonna delete it yet
                                   r: RBNode[TK, TV] = right,
                                   c: RBColor = clr,
                                   k: TK = key,
                                   v: TV = value) =
    if (black && l.red && r.red) //slurp (split "4-node")
      RBN(l.asBlack, Red, k, v, r asBlack)
    else if (r.red) //l.B-> rotate left, switch colors
      RBN(RBN(l, Red, k, v, r.left), c, r key, r value, r right)
    else if (black && l.red && l.left.red) //split line and rotate
      RBN(l.left.asBlack, Red, l key, l value, RBN(l right, Black, k, v, r))
    else
      RBN(l, c, k, v, r)

  //invariant: deleted node (this) is always Red
  def withoutFirst: RBNode[K, TVal] = if (black) throw new Exception("Invariant broken: must be red") else if (isRedLeaf) RBNil else if (left red) copy(left withoutFirst) //no need to balance
  else if (left blb)
    if (right blr) {
      val RBN(l, _, k, v, r) = right.left
      RBN(RBN(left.asRed.withoutFirst, Black, key, value, l), Red, k, v, right copy r)
    } else balanceRight(left.asRed.withoutFirst, right.asRed, Black) //balanceRight(right.asRed, left.asRed.withoutFirst, Black) //it is correct order
  else copy(left copy left.left.withoutFirst) //skip, left.left cannot be but red

  //invariant: deleted node (this) is always Red
  def withoutLast: RBNode[K, TVal] = {
    //stabilization for invariant, rotate and balance
    def withoutLastBalance(n: RBNode[K, TVal]) = n.left.balanceRight(n.copy(n.left.right, Red) withoutLast, c = n clr)

    if (black) throw new Exception("Invariant broken: must be red") else if (isRedLeaf) RBNil
    else if (left red) withoutLastBalance(this)
    else if (right blb)
      if (left blr)
        left copy (left.left.asBlack, Red, r = balanceRight(right.asRed withoutLast, left right, Black))
      else balanceRight(right.asRed.withoutLast, left asRed, Black)
    else copy(r = withoutLastBalance(right)) //skip
  }

  override def toString() = "(%s [%s %s: %s] %s)" format (left, clr, key, value, right)

  def htmlDump: scala.xml.Elem =
    <table class="tg">
      <tr>
        <th colspan="2"><div class={ clr.toString toLowerCase }>{ key } → { value }</div></th>
      </tr>
      <tr>
        <td>{ left htmlDump }</td>
        <td>{ right htmlDump }</td>
      </tr>
    </table>
}

object RBNode {
  def unapply[K, B](n: RBNode[K, B]) = if (n isNil) None else Some((n key, n clr, n value, n left, n right))

  def mkLeaf[K, V](k: K, v: V) = RBN(RBNil, Red, k, v, RBNil)
  def mkBlackLeaf[K, V](k: K, v: V) = RBN(RBNil, Black, k, v, RBNil)
}

case class RBN[K, V](left: RBNode[K, V], clr: RBColor, key: K, value: V, right: RBNode[K, V]) extends RBNode[K, V]

case object RBNil extends RBNode[Nothing, Nothing] {
  override def clr = Black
  override def key: Nothing = throw new NoSuchElementException("RBNil.key")
  override def value: Nothing = throw new NoSuchElementException("RBNil.value")
  override def isNil = true
  override def left = RBNil
  override def right = RBNil

  override def asRed = RBNil
  override def asBlack = RBNil

  override def toString() = "()"
  override def htmlDump = <div class="nil"></div>
}

trait RBColor {
  def ?(o: RBColor) = this equals o
  def unary_! = if (this ? Red) Black else Red
}
case object Red extends RBColor
case object Black extends RBColor










