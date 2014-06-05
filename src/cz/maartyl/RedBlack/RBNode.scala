package cz.maartyl.RedBlack

/**
 * Represents RBMap tree nodes and thus sub trees
 * 
 * @author  Maartyl
 * */

sealed abstract class RBNode[+K, +TVal] extends /*Tuple2[K, TVal](key, value) with*/ BinTreeNode[K, TVal] {
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
  def asOpposite: RBNode[K, TVal] = copy(c = !clr) //negates node color
  def hasLeft = left != RBNil
  def hasRight = right != RBNil
  def pair = (key, value)

  def isRedLeaf = !(black || hasLeft || hasRight) //red node with no actual nodes in subtrees (could be easily deleted)
  def blb = black && left.black //helper methods to make deletion code more readable
  def blr = black && left.red

  //works like clojure assoc; simplifies 'modifying'
  def copy[TK >: K, TV >: TVal](l: RBNode[TK, TV] = left, c: RBColor = clr,
                                k: TK = key, v: TV = value,
                                r: RBNode[TK, TV] = right) = RBN(l, c, k, v, r)

  //called to balance this subtree after right subtree has been changed 
  def balanceRight[TK >: K, TV >: TVal](nr: RBNode[TK, TV], // new right
                                        l: RBNode[TK, TV] = left, //optional changes of node contetns, in spirit of `copy`
                                        c: RBColor = clr,
                                        k: TK = key,
                                        v: TV = value) =
    if (c ? Black && l.red && nr.red) //slurp (split "4-node")
      RBN(l asBlack, Red, k, v, nr asBlack)
    else if (nr red) //l.B-> rotate left, switch colors
      RBN(RBN(l, Red, k, v, nr left), c, nr key, nr value, nr right)
    else
      RBN(l, c, k, v, nr)

  //called to balances this subtree after left subtree has been changed
  def balanceLeft[TK >: K, TV >: TVal](nl: RBNode[TK, TV], // new left
                                       r: RBNode[TK, TV] = right, //optional changes of node contetns, in spirit of `copy`
                                       c: RBColor = clr,
                                       k: TK = key,
                                       v: TV = value) =
    if (c ? Black && nl.red && nl.left.red) //split line and rotate
      RBN(nl.left asBlack, Red, nl key, nl value, RBN(nl right, Black, k, v, r))
    else
      RBN(nl, c, k, v, r)

  //rebalances subtree such that left-most node could be omitted (is RedLeaf) and ommits it
  //holds invariant: deleted node (this) is always Red
  def withoutFirst: RBNode[K, TVal] = if (black) throw new Exception("Invariant broken: must be red") else if (isRedLeaf) RBNil
  else if (left red) copy(left withoutFirst) //no need to balance
  else if (left blb)
    if (right blr) {
      val RBN(l, _, k, v, r) = right.left
      RBN(RBN(left.asRed.withoutFirst, Black, key, value, l), Red, k, v, right copy r)
    } else balanceRight(right.asRed, left.asRed.withoutFirst, Black) //it is correct order
  else copy(left copy left.left.withoutFirst) //skip, left.left cannot be but red // TODO: bug here?

  //rebalances subtree such that right-most node could be omitted (is RedLeaf) and ommits it
  //hold invariant: deleted node (this) is always Red
  def withoutLast: RBNode[K, TVal] = {
    //stabilization for invariant, rotate and balance
    def withoutLastBalance(n: RBNode[K, TVal]) = n.left.balanceRight(n.copy(n.left.right, Red)withoutLast, c = n clr)

    if (black) throw new Exception("Invariant broken: must be red") else if (isRedLeaf) RBNil
    else if (left red) withoutLastBalance(this)
    else if (right blb)
      if (left blr)
        left copy (left.left.asBlack, Red, r = balanceRight(right.asRed withoutLast, left right, Black))
      else balanceRight(right.asRed.withoutLast, left asRed, Black)
    else copy(r = withoutLastBalance(right)) //skip
  }

  override def toString() = "(%s [%s %s: %s] %s)" format (left, clr, key, value, right)

  def htmlDump: scala.xml.Elem = //creates HTML table representing this node; recursively creates dumps for subtrees 
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

object RBNode { //just helper methods to create nodes
  def mkLeaf[K, V](k: K, v: V) = RBN(RBNil, Red, k, v, RBNil)
  def mkBlackLeaf[K, V](k: K, v: V) = RBN(RBNil, Black, k, v, RBNil)
}

//main node implementation; all methods are inherited from RBNode
case class RBN[K, V](left: RBNode[K, V], clr: RBColor, key: K, value: V, right: RBNode[K, V]) extends RBNode[K, V]

//stub implementation of RBNode, used to represent empty nodes; handles edge conditions
case object RBNil extends RBNode[Nothing, Nothing] {
  override def clr = Black
  override def key: Nothing = throw new NoSuchElementException("RBNil.key")
  override def value: Nothing = throw new NoSuchElementException("RBNil.value")
  override def isNil = true
  override def left = RBNil
  override def right = RBNil

  override def asRed = throw new UnsupportedOperationException("RBNil cannot become red")
  override def asBlack = RBNil

  override def toString() = "()"
  override def htmlDump = <div class="nil"></div>
}

//mainly just enum to represent RBNode type; contains simple helper methods
sealed trait RBColor {
  def ?(o: RBColor) = this equals o //== misbehaves and equals is long
  def unary_! = if (this ? Red) Black else Red
}
case object Red extends RBColor
case object Black extends RBColor










