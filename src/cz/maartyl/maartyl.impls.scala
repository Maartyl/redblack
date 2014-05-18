package cz

package object maartyl {
  object Pipe {
    implicit class PipedObject[A](val value: A) extends AnyVal {
      def |>[B](f: A => B): B = f(value)
      def |>[B](f: (A, A) => B): B = f(value, value)
      def |>[B](f: (A, A, A) => B): B = f(value, value, value)
    }
  }
  object Regex {
    implicit class Regex(val sc: StringContext) extends AnyVal{
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail map { _ => "x" }: _*)
    }
  }
}