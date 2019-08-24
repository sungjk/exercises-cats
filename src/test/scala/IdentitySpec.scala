import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-24.
  */
class IdentitySpec extends FunSuite with Matchers {
    import cats._
    val x: Id[Int] = 1
    val y: Int = x

    test("Id type") {
        val anId: Id[Int] = 42
        anId should be(42)
    }

    import cats.Functor
    val one: Int = 1
    Functor[Id].map(one)(_ + 1)

    test("Id has pure") {
        Applicative[Id].pure(42) should be(42)
    }

    def map[A, B](fa: Id[A])(f: A => B): Id[B] = ???
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = ???
    def coflatMap[A, B](fa: Id[A])(f: Id[A] => B): Id[B] = ???

    test("Id Comonad") {
        val fortytwo: Int = 42
        Comonad[Id].coflatMap(fortytwo)(_ + 1) should be(43)
    }
}
