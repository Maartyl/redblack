package cz.maartyl

object Pipe { //reverse order apply helper
  implicit class PipedObject[A](val value: A) extends AnyVal {
    @inline def |>[B](f: A => B): B = f(value)
    @inline def |>[B](f: (A, A) => B): B = f(value, value)
    @inline def |>[B](f: (A, A, A) => B): B = f(value, value, value)
  }
}

object Regex { //string to regex helper; usage: case r"([^ ]${ assignToThisVal }rest.*" => println(assignToThisVal) ...
  implicit class Regex(val sc: StringContext) extends AnyVal {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail map { _ => "x" }: _*)
  }
}
