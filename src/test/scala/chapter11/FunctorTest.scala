package chapter11

import org.scalacheck.Prop
import org.scalatest.{Matchers, FlatSpec}

class FunctorTest extends FlatSpec with Matchers {
  "A functor" should "obey the functor laws" in {
    // map(x)(a => a) == x

    val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
    }

    Prop.forAll {
      ls: List[Int] => listFunctor.map(ls)(a => a) == ls
    }

  }
}
