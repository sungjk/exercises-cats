import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-21.
  */
class MonadSpec extends FunSuite with Matchers {
    import cats._
    import cats.implicits._

    test("flatten function") {
        Option(Option(1)).flatten should be(Option(1))
        Option(None).flatten should be(None)
        List(List(1), List(2, 3)).flatten should be(List(1, 2, 3))
    }

    implicit def optionMonad(implicit app: Applicative[Option]): Monad[Option] = new Monad[Option] {
        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = app.map(fa)(f).flatten
        override def pure[A](a: A): Option[A] = app.pure(a)
        override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
    }

    test("monad instances") {
        Monad[Option].pure(42) should be(Option(42))
    }

    implicit val listMonad = new Monad[List] {
        override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
        override def pure[A](a: A): List[A] = List(a)
        override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
    }

    test("flatmap function") {
        Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) should be(List(1, 1, 2, 2, 3, 3))
    }

    test("ifM function") {
        Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(Option("truthy"))
        Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4, 1, 2))
    }

    test("monad composition") {
        case class OptionT[F[_], A](value: F[Option[A]])

        implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[OptionT[F, ?]] = {
            new Monad[OptionT[F, ?]] {
                def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
                def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
                    OptionT {
                        F.flatMap(fa.value) {
                            case None => F.pure(None)
                            case Some(a) => f(a).value
                        }
                    }
                def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
                    OptionT(
                        F.tailRecM(a) { a0 =>
                            F.map(f(a).value) {
                                _.fold(Either.right[A, Option[B]](None))(_.map(b => Some(b): Option[B]))
                            }
                        }
                    )
            }
        }

        optionTMonad[List].pure(42) should be(OptionT(List(Option(42))))
    }
}
