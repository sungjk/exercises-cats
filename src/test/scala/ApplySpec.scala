import cats.Apply
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-19.
  */
class ApplySpec extends FunSuite with Matchers {
    /**
      * ap: takes type F[A => B]
      * map: takes type A => B
      */
    implicit val optionApply: Apply[Option] = new Apply[Option] {
        override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
            fa.flatMap(a => f.map(ff => ff(a)))

        override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
            fa map f

        override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
            fa.flatMap(a => fb.map(b => (a, b)))
    }

    implicit val listApply: Apply[List] = new Apply[List] {
        override def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
            fa.flatMap(a => f.map(ff => ff(a)))

        override def map[A, B](fa: List[A])(f: A => B): List[B] =
            fa map f

        override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
            fa.zip(fb)
    }

    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    val addTwo: Int => Int = _ + 2
    val addArity2 = (a: Int, b: Int) => a + b
    val addArity3 = (a: Int, b: Int, c: Int) => a + b + c

    test("extends functor") {
        Apply[Option].map(Some(1))(intToString) should be(Option("1"))
        Apply[Option].map(Some(1))(double) should be(Option(2))
        Apply[Option].map(None)(addTwo) should be(None)
    }

    test("is composable") {
        val listOpt = Apply[List] compose Apply[Option]
        val plusOne = (x: Int) => x + 1
        listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(List(Some(2), None, Some(4)))
    }

    test("ap method") {
        Apply[Option].ap(Some(intToString))(Some(1)) should be(Some("1"))
        Apply[Option].ap(Some(double))(Some(1)) should be(Some(2))
        Apply[Option].ap(Some(double))(None) should be(None)
        Apply[Option].ap(None)(Some(1)) should be(None)
        Apply[Option].ap(None)(None) should be(None)
    }

    // 하나라도 None이면 result도 None
    test("apN method") {
        Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) should be(Some(3))
        Apply[Option].ap2(Some(addArity2))(Some(1), None) should be(None)
        Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))
    }

    test("mapN method") {
        Apply[Option].map2(Some(1), Some(2))(addArity2) should be(Some(3))
        Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) should be(Some(6))
    }

    test("tupleN method") {
        Apply[Option].tuple2(Some(1), Some(2)) should be(Some((1, 2)))
        Apply[Option].tuple3(Some(1), Some(2), Some(3)) should be(Some((1, 2, 3)))
    }

    test("builder syntax") {
        import cats.implicits._
        val option2 = Option(1) |@| Option(2)
        val option3 = option2 |@| Option.empty[Int]

        option2 map addArity2 should be(Some(3))
        option3 map addArity3 should be(None)

        option2 apWith Some(addArity2) should be(Some(3))
        option3 apWith Some(addArity3) should be(None)

        option2.tupled should be(Option((1, 2)))
        option3.tupled should be(None)
    }
}
