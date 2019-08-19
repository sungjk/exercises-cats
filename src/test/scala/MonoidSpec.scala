import cats.kernel.Monoid
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-19.
  */
class MonoidSpec extends FunSuite with Matchers {
    import cats.implicits._

    test("has a empty operation") {
        Monoid[String].empty should be("")
        Monoid[String].combineAll(List("a", "b", "c")) should be("abc")
        Monoid[String].combineAll(List()) should be("")
    }

    test("advantages of using monoid operations") {
        val aMap: Map[String, Int] = Map("a" -> 4, "b" -> 2)
        val anotherMap: Map[String, Int] = Map()

        Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) should be(aMap)
        Monoid[Map[String, Int]].combineAll(List()) should be(anotherMap)
    }

    test("accumulating with a monoid on foldMap") {
        val l = List(1, 2, 3, 4, 5)
        l.foldMap(identity) should be(15)
        l.foldMap(i => i.toString) should be("12345")
    }

    test("accumulating with a tuple on foldMap") {
        val l = List(1, 2, 3, 4, 5)
        l.foldMap(i => (i, i.toString)) should be(15, "12345")
    }
}
