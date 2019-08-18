import cats.kernel.Semigroup
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-18.
  */
class SemigroupSpec extends FunSuite with Matchers {
    import cats.implicits._

    test("has a combine operation") {
        Semigroup[Int].combine(1, 2) should be(3)
        Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
        Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))
        Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))
    }

    test("has an advanced combine operation") {
        Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(67)
    }

    test("composes with other semigroups") {
        val aMap = Map("foo" -> Map("bar" -> 5))
        val anotherMap = Map("foo" -> Map("bar" -> 6))
        val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
        combinedMap.get("foo") should be(Some(Map("bar" -> 11)))
    }

    test("has special syntax") {
        val one: Option[Int] = Option(1)
        val two: Option[Int] = Option(2)
        val n: Option[Int]   = None

        one |+| two should be(Option(3))
        n |+| two should be(Option(2))
        n |+| n should be(None)
        two |+| n should be(Option(2))
    }
}
