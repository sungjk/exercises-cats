import cats.Functor
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-19.
  */
class FunctorSpec extends FunSuite with Matchers {
    import cats.implicits._

    test("using functor") {
        Functor[Option].map(Option("Hello"))(_.length) should be(Option(5))
        Functor[Option].map(None: Option[String])(_.length) should be(None)
    }

    test("lifting to a functor") {
        val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
        lenOption(Some("Hello")) should be(Option(5))
    }

    test("using fproduct") {
        val source = List("Cats", "is", "awesome")
        val product = Functor[List].fproduct(source)(_.length).toMap
        product.getOrElse("Cats", 0) should be(4)
        product.getOrElse("is", 0) should be(2)
        product.getOrElse("awesome", 0) should be(7)
    }

    test("composing functors") {
        val listOpt = Functor[List] compose Functor[Option]
        listOpt.map(List(Some(1), None, Some(3)))(_ + 1) should be(List(Option(2), None, Option(4)))
    }
}
