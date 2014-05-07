package cz.maartyl.RedBlack

// 2-3 RedBlack Tree : onle left child could ever be red...

import scala.annotation.tailrec

class RBMap[K, B](
  val root: RBNode[K, B],
  override val size: Int)(implicit val ordering: Ordering[K]) extends BinTree[K, B] {
  protected[this] def builderCreate = RBMap.newBuilder[K, B]
  type Node = RBNode[K, B]
  def mapnil = RBMap()
  override def toString = "RBMap(%s: %s)".format(size, root)

  def firstNode: Node = {
    @tailrec def loop(n: Node): Node = if (n.hasLeft) loop(n.left) else n
    loop(root)
  }
  def lastNode: Node = {
    @tailrec def loop(n: Node): Node = if (n.hasRight) loop(n.right) else n
    loop(root)
  }
  def findNode(key: K): Option[Node] = findNode(key, root)
  @tailrec private def findNode(key: K, n: Node): Option[Node] =
    ordering.compare(key, n.key) match {
      case c if c < 0 => if (n.hasLeft) findNode(key, n.left) else None
      case c if c > 0 => if (n.hasRight) findNode(key, n.right) else None
      case _ => Some(n)
    }

  private def copy[B1](root: RBNode[K, B1], inc: Int) = new RBMap[K, B1](root, size + inc)(ordering)

  //returns new tree without node with given key, if present, otherwise itself
  def without(key: K): BinTree[K, B] = {

    def recur(n: Node): Node = {
      n
    }

    val changed = contains(key)
    copy(recur(root), if (changed) -1 else 0)
  }

  //returns new tree with node added/changed (conjugate)
  def conj[B1 >: B](key: K, value: B1): BinTree[K, B1] = {
    val vv = value.asInstanceOf[B]
    var changed = true //number of elements //could have just called contains... +1 lookup...

    def recur(n: Node): Node =
      if (n.isNil) RBNode.mkLeaf(key, vv) else {
        ordering.compare(key, n.key) match {
          case c if c < 0 => n.balanceLeft(recur(n.left))
          case c if c > 0 => n.balanceRight(recur(n.right))
          case _ => { //just new value
            changed = false;
            val RBN(l, clr, k, _, r) = n;
            RBN(l, clr, k, vv, r)
          } 
        }
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

  def htmlDump = RBMap.htmlDumpBase(RBNil.htmlDump)
  def traverse[T, A, D, C](pref: T => A, inf: T => D, postf: T => C, transform: BinTreeNode[K, B] => T = identity _): (Stream[A], Stream[D], Stream[C]) =
    (Stream.Empty, Stream.Empty, Stream.Empty)
}

object RBMap extends BinTreeObj {
  def empty[A, B](implicit ord: Ordering[A]) = new RBEmpty[A, B]()(ord)

  def htmlDumpBase(tree: scala.xml.Elem) =
    """<!DOCTYPE HTML>
<html><head><style>
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












