import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-21.
  */
class ApplicativeSpec extends FunSuite with Matchers {
    /**
      * takes any value and returns the value in the context of the functor.
      */
    import cats._
    import cats.implicits._

    test("pure method") {
        Applicative[Option].pure(1) should be(Some(1))
        Applicative[List].pure(1) should be(List(1))
    }

    test("applicative composition") {
        (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Some(1)))
    }

    test("applicative and monad") {
        Monad[Option].pure(1) should be(Some(1))
        Applicative[Option].pure(1) should be(Some(1))
    }
}
