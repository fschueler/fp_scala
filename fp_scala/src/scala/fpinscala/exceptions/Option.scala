package fpinscala.exceptions

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]