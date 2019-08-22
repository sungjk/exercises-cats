import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-22.
  */
class FoldableSpec extends FunSuite with Matchers {
    import cats._
    import cats.implicits._

    test("foldLeft function") {
        Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) should be(6)
        Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) should be("abc")
    }

    test("foldRight function") {
        val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) => Later(x + rest.value))
        lazyResult.value should be(6)
    }

    test("fold function") {
        Foldable[List].fold(List("a", "b", "c")) should be("abc")
        Foldable[List].fold(List(1, 2, 3)) should be(6)
    }

    test("foldMap function") {
        Foldable[List].foldMap(List("a", "b", "c"))(_.length) should be(3)
        Foldable[List].foldMap(List(1, 2, 3))(_.toString) should be("123")
    }

    test("find function") {
        Foldable[List].find(List(1, 2, 3))(_ > 2) should be(Some(3))
        Foldable[List].find(List(1, 2, 3))(_ > 5) should be(None)
    }

    test("exists function") {
        Foldable[List].exists(List(1, 2, 3))(_ > 2) should be(true)
        Foldable[List].exists(List(1, 2, 3))(_ > 5) should be(false)
    }

    test("forall function") {
        Foldable[List].forall(List(1, 2, 3))(_ <= 3) should be(true)
        Foldable[List].forall(List(1, 2, 3))(_ < 3) should be(false)
    }

    test("foldK function") {
        Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(List(1, 2, 3, 4, 5))
        Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(Some("two"))
    }

    test("toList function") {
        Foldable[List].toList(List(1, 2, 3)) should be(List(1, 2, 3))
        Foldable[Option].toList(Option(42)) should be(List(42))
        Foldable[Option].toList(None) should be(List())
    }

    test("filter_ function") {
        Foldable[List].filter_(List(1, 2, 3))(_ < 3) should be(List(1, 2))
        Foldable[Option].filter_(Option(42))(_ != 42) should be(List())
    }

    test("traverse_ function") {
        def parseInt(s: String): Option[Int] =
            Either.catchOnly[NumberFormatException](s.toInt).toOption

        Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be(Some())
        Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be(None)
    }

    test("compose foldables") {
        val FoldableListOption = Foldable[List].compose[Option]
        FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be(10)
        FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be("123")
    }

    test("foldable methods") {
        Foldable[List].isEmpty(List(1, 2, 3)) should be(false)
        Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2) should be(List(2, 3))
        Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2) should be(List(1))
    }
}
