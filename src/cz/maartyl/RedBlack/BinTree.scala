package cz.maartyl.RedBlack

import scala.collection.immutable.{ MapLike, SortedMap }
import scala.collection.SortedMapLike
import scala.collection.mutable.Builder
import scala.collection.generic.ImmutableSortedMapFactory
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.collection.immutable.Queue

/**
 * This is just a stub to declutter main RBMap from inheritance and stuff.
 * Also Programs will use this Trait for type, thus will be free from implementation details.
 * Implements non-intersting, interop with Scala libraries.
 * 
 * Based on TreeMap Scala 
 * 
 * Also: Interface type: One shouldn't work with implementations anyway...
 *
 */

trait BinTree[K, +B]
  extends SortedMap[K, B]
  with SortedMapLike[K, B, BinTree[K, B]]
  with MapLike[K, B, BinTree[K, B]]
  with Iterable[(K, B)]
  with Serializable {

  override def size: Int

  def mapnil: BinTree[K, B] //can't use empty directly 
  protected[this] def builderCreate: Builder[(K, B), BinTree[K, B]] //can't delegate newBuilder directly

  def firstNode: BinTreeNode[K, B] //smallest key //could return null in empty Tree
  def lastNode: BinTreeNode[K, B] //largest key
  def without(key: K): BinTree[K, B] //returns new tree without given key 
  def findNode(key: K): Option[BinTreeNode[K, B]]
  def conj[B1 >: B](key: K, value: B1): BinTree[K, B1] //returns new tree with node added/changed (conjugate)

  override def iterator: Iterator[(K, B)]
  
  def traverse[T, T1, T2, T3](pref: T => T1, inf: T => T2, postf: T => T3, transform: BinTreeNode[K, B] => T = identity _): (Stream[T1], Stream[T2], Stream[T3])
  def htmlDump() : String

  //implemented methods:

  override protected[this] def newBuilder: Builder[(K, B), BinTree[K, B]] = builderCreate

  override def get(key: K) = findNode(key) map { _.value }
  override def contains(key: K) = findNode(key).isDefined
  override def isDefinedAt(key: K): Boolean = contains(key)
  override def firstKey = firstNode.key
  override def lastKey = lastNode.key

  override def head = {
    val smallest = firstNode
    (smallest.key, smallest.value)
  }
  override def headOption = if (isEmpty) None else Some(head)
  override def last = {
    val greatest = lastNode
    (greatest.key, greatest.value)
  }
  override def lastOption = if (isEmpty) None else Some(last)

  override def isEmpty: Boolean = size == 0 //Trees remember their size, doesn't have to count
  override def tail = without(firstKey)
  override def init = without(lastKey)
  override def empty = mapnil

  //drop take slice: very unefficient, but weird anyway
  override def drop(n: Int) = {
    if (n <= 0) this
    else if (n >= size) empty
    var list = this
    for (_ <- 0 to n) list = list.tail
    list
  }
  override def take(n: Int) = {
    if (n <= 0) empty
    else if (n >= size) this
    var list = this
    for (_ <- 0 to (size - n)) list = list.init
    list
  }
  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else drop(from).take(until - from)
  }

  override def dropRight(n: Int) = take(size - n)
  override def takeRight(n: Int) = drop(size - n)
  override def splitAt(n: Int) = (take(n), drop(n))

  private[this] def countWhile(p: ((K, B)) => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next)) result += 1
    result
  }
  override def dropWhile(p: ((K, B)) => Boolean) = drop(countWhile(p))
  override def takeWhile(p: ((K, B)) => Boolean) = take(countWhile(p))
  override def span(p: ((K, B)) => Boolean) = splitAt(countWhile(p))

  /**
   * A new BinTree with the entry added is returned,
   *  assuming that key is not in the BinTree.
   */
  def insert[B1 >: B](key: K, value: B1): BinTree[K, B1] = {
    assert(!contains(key))
    updated(key, value).asInstanceOf[BinTree[K, B1]]
  }
  /**
   * A new BinTree with the entry added is returned,
   *  if key is not in the BinTree, otherwise
   *  the key is updated with the new entry.
   */
  override def updated[B1 >: B](key: K, value: B1): BinTree[K, B1] = conj(key, value)

  override def +[B1 >: B](kv: (K, B1)): BinTree[K, B1] = updated(kv._1, kv._2)
  override def ++[B1 >: B](xs: GenTraversableOnce[(K, B1)]): BinTree[K, B1] =
    ((repr: BinTree[K, B1]) /: xs.seq)(_ + _)

  def -(key: K): BinTree[K, B] = without(key)

  // TODO: (rangeImpl) find out what this does and implement
  override def rangeImpl(from: Option[K], until: Option[K]): BinTree[K, B] = this // = new BinTree[A, B](RB.rangeImpl(tree, from, until))
}

trait BinTreeNode[+A, +B] {
  def key: A
  def value: B
}

trait BinTreeObj extends ImmutableSortedMapFactory[BinTree] {
  def empty[A, B](implicit ord: Ordering[A]): BinTree[A, B]

  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), BinTree[A, B]] = new SortedMapCanBuildFrom[A, B]
}

















