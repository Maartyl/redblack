package cz.maartyl.RedBlack

import scala.annotation.tailrec

class RBMap[K, B](
  val root: RBNode[K, B],
  override val size: Int)(implicit val ordering: Ordering[K]) extends BinTree[K, B] {
  protected[this] def builderCreate = RBMap.newBuilder[K, B]
  type Node = RBNode[K, B]
  def mapnil = RBMap()
  override def toString = "RBMap(%s: %s)".format(size, root)

  def firstNode: Node = firstNode(root)
  @tailrec private def firstNode(n: Node): Node = if (n.hasLeft) firstNode(n.left) else n

  def lastNode: Node = lastNode(root)
  @tailrec private def lastNode(n: Node): Node = if (n.hasRight) lastNode(n.right) else n

  def findNode(key: K): Option[Node] = findNode(key, root)
  @tailrec private def findNode(key: K, n: Node): Option[Node] = if (n.isNil) None else
    ordering.compare(key, n.key) match {
      case c if c < 0 => findNode(key, n.left)
      case c if c > 0 => findNode(key, n.right)
      case _ => Some(n)
    }

  private def copy[B1](root: RBNode[K, B1], inc: Int) = if (size + inc > 0) new RBMap[K, B1](root, size + inc)(ordering) else new RBEmpty[K, B1]()(ordering)
  def copy = copy[B](root, 0) //public, not exported to BinTree interface

  def withoutFirst: BinTree[K, B] = copy[B](withoutFirst(root), -1)
  def withoutLast: BinTree[K, B] = copy(withoutLast(root), -1)
  private def withoutFirst(n: Node): Node = n.asRed.withoutFirst.asBlack
  private def withoutLast(n: Node): Node = n.asRed.withoutLast.asBlack

  //returns new tree without node with given key, if present, otherwise itself
  def without(key: K): BinTree[K, B] = findNode(key) match {
    case None => this
    case Some(_) => {
      def recur(n: Node): Node = if (n.isNil) RBNil else
        ordering.compare(key, n.key) match {
          case c if c < 0 => if (n.left.blb)
            if (n.right.blr)
              n.right.left.copy(n.copy(recur(n.left.asRed), Black, r = n.right.left.left), Red, r = n.right.copy(n.right.left.right, Black))
            else
              n.balanceRight(n.right.asRed, recur(n.left.asRed), Black)
          else
            n.copy(l = recur(n.left))

          case c if c > 0 => if (n.left.red)
            n.left.balanceRight(recur(n.copy(n.left.right, Red)), n.left.left, n.clr)
          else if (n.red && n.right.blb)
            if (n.left.blr)
              n.left.copy(n.left.left.asBlack, Red, r = n.balanceRight(recur(n.right.asRed), n.left.right, Black))
            else
              n.balanceRight(recur(n.right.asRed), n.left.asRed, Black)
          else
            n.copy(r = recur(n.right))

          case _ => if (n.isRedLeaf) RBNil else if (n.left.red) n.left.balanceRight(recur(n.copy(n.left.right, Red)), c = n.clr) else {
            val (mk, mv) = firstNode(n.right).pair //has next, successor to swap
            if (n.left.blb)
              if (n.right.blr)
                n.left.balanceRight(n.balanceRight(n.right.asRed.withoutFirst, n.left.right, Black, mk, mv), n.left.left.asBlack, Red)
              else
                n.balanceRight(n.right.asRed.withoutFirst, n.left.asRed, Black, mk, mv)
            else RBN(n.left, Red, mk, mv, n.right.copy(n.right.left.asRed.withoutFirst, Black)) //asRed is wrong here
          }
        }

      copy(recur(root.asRed).asBlack, -1)
    }
  }

  //returns new tree with node added/changed (conjugate)
  def conj[B1 >: B](key: K, value: B1): BinTree[K, B1] = {
    val vv = value.asInstanceOf[B]
    var changed = true //number of elements //could have just called contains... +1 lookup...

    def recur(n: Node): Node =
      if (n.isNil) RBNode.mkLeaf(key, vv) else
        ordering.compare(key, n.key) match {
          case c if c < 0 => n.balanceLeft(recur(n.left))
          case c if c > 0 => n.balanceRight(recur(n.right))
          case _ => n.copy(v = { changed = false; vv }) //just new value // more readable
          //case _ => { changed = false; n.copy(v = vv) } //just new value
        }
    val newroot = recur(root).asBlack //side effect: order enforcement (why splitted into 2 lines)
    copy(newroot, if (changed) 1 else 0)
  }

  def htmlDump = RBMap.htmlDumpBase(root.htmlDump)

  def traverse[T, T1, T2, T3](pref: T => T1, inf: T => T2, postf: T => T3, transform: BinTreeNode[K, B] => T = identity _): (Stream[T1], Stream[T2], Stream[T3]) = {
    //lazy traverse: stupid, but funny idea 
    import scala.collection.mutable.Queue
    val a = Queue[() => T1]()
    val b = Queue[() => T2]()
    val c = Queue[() => T3]()

    def recur(n: Node): Unit = {
      lazy val t = transform(n)
      a.enqueue(() => { pref(t) })
      if (n.hasLeft) recur(n.left)
      b.enqueue(() => { inf(t) })
      if (n.hasRight) recur(n.right)
      c.enqueue(() => { postf(t) })
    }
    recur(root)
    def f[A](q: Queue[() => A]) = q.toStream map { _() }
    (f(a), f(b), f(c))
  }

  override def iterator: Iterator[(K, B)] = nodeIterator map { _.pair }
  def nodeIterator: Iterator[Node] = new NodeIterator()
  class NodeIterator extends Iterator[Node] {
    //emulates recursion
    private val stack = collection.mutable.Stack[Node]()
    push(root)

    @tailrec private def push(n: Node): Unit =
      if (!n.isNil) {
        stack.push(n)
        push(n.left)
      }

    def hasNext: Boolean = !stack.isEmpty
    def next: Node = {
      val n = stack.pop
      push(n.right)
      n
    }
  }

}

//so I don't need to take care of 'null root'
class RBEmpty[K, B]()(implicit val ordering: Ordering[K]) extends BinTree[K, B] {
  import scala.collection.AbstractIterator
  override def size = 0
  protected[this] def builderCreate = RBMap.newBuilder[K, B]
  type Node = RBNode[K, B]
  def mapnil = RBMap()

  def firstNode: Node = throw new IllegalAccessException("(empty RBMap).first")
  def lastNode: Node = throw new IllegalAccessException("(empty RBMap).last")
  def findNode(key: K): Option[Node] = None
  def without(key: K): BinTree[K, B] = this
  override def iterator: Iterator[(K, B)] = new Iterator[(K, B)] {
    def hasNext = false
    def next = null
  }
  //returns actual tree with node added (conjugate)
  def conj[B1 >: B](key: K, value: B1): BinTree[K, B1] =
    new RBMap[K, B1](RBNode.mkBlackLeaf(key, value), 1)(ordering)

  def withoutFirst: BinTree[K, B] = throw new IllegalAccessException("(empty RBMap).first")
  def withoutLast: BinTree[K, B] = throw new IllegalAccessException("(empty RBMap).last")

  def htmlDump = RBMap.htmlDumpBase(RBNil.htmlDump)
  def traverse[T, A, D, C](pref: T => A, inf: T => D, postf: T => C, transform: BinTreeNode[K, B] => T = identity _): (Stream[A], Stream[D], Stream[C]) =
    (Stream.Empty, Stream.Empty, Stream.Empty)
}

object RBMap extends BinTreeObj {
  def empty[A, B](implicit ord: Ordering[A]) = new RBEmpty[A, B]()(ord)

  def htmlDumpBase(tree: scala.xml.Elem) =
    """<!DOCTYPE HTML><html><head><style>
	body {
		background: #888888 ;
		color: white;
		}
	table {  }
	div {
		width: 100%;
		height: 100%;
	}
	.red { background-color: #AA0000;}
	.black {background-color: #000000;}
	.nil {
		background-color: #000000;
		height: 5px;
	}

	.tg  {
		border-collapse:collapse;
		border-spacing:0;
		font-family: sans-serif;
		padding: 0px;
		margin: 0px;
		border-width:0px;
		overflow:hidden;
	}
	.tg td {
		min-width: 40px;
	 }
	.tg th {
		text-align:center;
		font-size:26px;
		padding: 0px;
	}
	.tg tbody {
		vertical-align: top;
	}
</style></head><body>
	""" + tree.toString + """
</body></html>"""
}












