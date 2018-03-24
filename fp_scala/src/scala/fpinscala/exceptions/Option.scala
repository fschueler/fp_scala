package fpinscala.exceptions

trait Option[+A] {

  // map can only "forward" None and Some
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // flatMap can forward None but can also turn a Some into a None! (f can fail)
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // extracts the value from a Some(v) or returns the default if the option is None
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  /* - map(f) returns an Option[Option[B]]
   * - getOrElse checks if we have Some[Option[B]] or None
   *   - in the case of Some[Option[B]] we extract the inner option
   *     which can be Some[B] or None (depending on the result of f)
   *   - in the case of None, we forward that 
   */
  def flatMap2[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse None

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]