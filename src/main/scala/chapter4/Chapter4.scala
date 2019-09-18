package chapter4

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
}

case class Some[+A](val value: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(value))
    override def flatMap[B](f: A => Option[B]): Option[B] = f(value) match {
        case mappedOption @ Some(_) => mappedOption
        case None => None
    }
    def getOrElse[B >: A](default: => B): B = value
    def orElse[B >: A](ob: => Option[B]): Option[B] = this
    def filter(f: A => Boolean): Option[A] = if (f(value)) this else None
    
}

case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = this
    def flatMap[B](f: Nothing => Option[B]): Option[B] = this
    def getOrElse[B >: Nothing](default: => B): B = default
    def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
    def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Excercise2 {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.length > 0 ) Some(xs.sum / xs.length) else None
    
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
}