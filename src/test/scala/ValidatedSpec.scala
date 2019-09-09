import cats.kernel.Semigroup
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-09-03.
  */
object ValidatedHelpers {
    case class ConnectionParams(url: String, port: Int)
//    for {
//        url <- config[String]("url")
//        port <- config[Int]("port")
//    } yield ConnectionParams(url, port)


    trait Read[A] {
        def read(s: String): Option[A]
    }

    object Read {
        def apply[A](implicit A: Read[A]): Read[A] = A

        implicit val stringRead: Read[String] =
            new Read[String] { def read(s: String): Option[String] = Some(s) }

        implicit val intRead: Read[Int] =
            new Read[Int] {
                def read(s: String): Option[Int] =
                    if (s.matches("-?[0-9]+")) Some(s.toInt)
                    else None
            }
    }

    sealed abstract class ConfigError
    final case class MissingConfig(field: String) extends ConfigError
    final case class ParseError(field: String) extends ConfigError

    sealed abstract class Validated[+E, +A]
    object Validated {
        final case class Valid[+A](a: A) extends Validated[Nothing, A]
        final case class Invalid[+E](e: E) extends Validated[E, Nothing]
    }

    import cats.data.Validated.{Invalid, Valid}

    case class Config(map: Map[String, String]) {
        def parse[A: Read](key: String): cats.data.Validated[ConfigError, A] =
            map.get(key) match {
                case None => Invalid(MissingConfig(key))
                case Some(value) =>
                    Read[A].read(value) match {
                        case None => Invalid(ParseError(key))
                        case Some(a) => Valid(a)
                    }
            }
    }

    def parallelValidate[E: Semigroup, A, B, C](v1: cats.data.Validated[E, A], v2: cats.data.Validated[E, B])(f: (A, B) => C): cats.data.Validated[E, C] =
        (v1, v2) match {
            case (Valid(a), Valid(b)) => Valid(f(a, b))
            case (Valid(_), i @ Invalid(_)) => i
            case (i @ Invalid(_), Valid(_)) => i
            case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }


    import cats.SemigroupK
    import cats.data.NonEmptyList
    import cats.implicits._

    implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
        SemigroupK[NonEmptyList].algebra[ConfigError]
    implicit val readString: Read[String] = Read.stringRead
    implicit val readInt: Read[Int] = Read.intRead

}

class ValidatedSpec extends FunSuite with Matchers {
    import ValidatedHelpers.Config
    import ValidatedHelpers.parallelValidate
    import ValidatedHelpers.ConnectionParams
    import ValidatedHelpers.MissingConfig
    import ValidatedHelpers.ParseError
    import ValidatedHelpers.ConfigError

    test("with no errors") {
        val config = Config(Map(("url", "127.0.0.1"), ("port", "1337")))

        val valid = parallelValidate(
            config.parse[String]("url").toValidatedNel,
            config.parse[Int]("port").toValidatedNel
        )(ConnectionParams.apply)

        valid.isValid should be(true)
        valid.getOrElse(ConnectionParams("", 0)) should be(ConnectionParams("127.0.0.1", 1337))
    }

    test("with accumulating errors") {
        val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "not a number")))

        val invalid = parallelValidate(
            config.parse[String]("url").toValidatedNel,
            config.parse[Int]("port").toValidatedNel,
        )(ConnectionParams.apply)

        import cats.data.Validated
        import cats.data.NonEmptyList

        invalid.isValid should be(false)
        val errors  = NonEmptyList(MissingConfig("url"), List(ParseError("port")))
        invalid == Validated.invalid(errors) should be(true)
    }


//    def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    import cats.Applicative
    import cats.data.Validated
    import cats.data.Validated.{Invalid, Valid}

    implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] =
        new Applicative[Validated[E, ?]] {
            override def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
                (fa, f) match {
                    case (Valid(a), Valid(fab)) => Valid(fab(a))
                    case (i @ Invalid(_), Valid(_)) => i
                    case (Valid(_), i @ Invalid(_)) => i
                    case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
                }

            override def pure[A](x: A): Validated[E, A] = Validated.valid(x)
            override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = fa.map(f)
            override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
                ap(fa.map(a => (b: B) => (a, b)))(fb)
        }

    import cats.Apply
    import cats.data.ValidatedNel
    import cats.SemigroupK
    import cats.data.NonEmptyList

    implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
        SemigroupK[NonEmptyList].algebra[ConfigError]

    val config = Config(Map(
        ("name", "cat"),
        ("age", "not a number"),
        ("houseNumber", "1234"),
        ("lane", "feline street")
    ))

    case class Address(houseNumber: Int, street: String)
    case class Person(name: String, age: Int, address: Address)

//    val personFromConfig: ValidatedNel[ConfigError, Person] =
//        Apply[ValidatedNel[ConfigError, ?]].map4(
//            config.parse[String]("name").toValidatedNel,
//            config.parse[Int]("age").toValidatedNel,
//            config.parse[Int]("house_number").toValidatedNel,
//            config.parse[String]("street").toValidatedNel
//        ) {
//            case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
//        }


    implicit def validatedMonad[E]: Monad[Validated[E, ?]] =
        new Monad[Validated[E, ?]] {
            def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] =
                fa match {
                    case Valid(a) => f(a)
                    case i @ Invalid(_) => i
                }

            def pure[A](x: A): Validated[E, A] = Valid(x)
            def tailRecM[A, B](a: A)(f: A => Validated[E, Either[A, B]]): Validated[E, B] = ???
        }

    trait Monad[F[_]] {
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
        def pure[A](x: A): F[A]
        def map[A, B](fa: F[A])(f: A => B): F[B] =
            flatMap(fa)(f.andThen(pure))
        def ap[A, B](fa: F[A])(f: F[A => B]): F[B] =
            flatMap(fa)(a => map(f)(fab => fab(a)))
    }


    test("sequential validation") {
        val config = Config(Map("house_number" -> "-42"))

        val houseNumber = config.parse[Int]("house_number").andThen { n =>
            if (n >= 0) Validated.valid(n)
            else Validated.invalid(ParseError("house_number"))
        }

        houseNumber.isValid should be(false)
        val error = ParseError("house_number")
        houseNumber == Validated.invalid(error) should be(true)
    }

    test("validation with either") {
        import cats.data.Validated
        import cats.implicits._

        def positive(field: String, i: Int): Either[ConfigError, Int] = {
            if (i >= 0) Either.right(i)
            else Either.left(ParseError(field))
        }

        val config = Config(Map("house_number" -> "-42"))
        val houseNumber: Validated[ConfigError, Int] = config.parse[Int]("house_number").withEither { either: Either[ConfigError, Int] =>
            either.flatMap { i =>
                positive("house_number", i)
            }
        }

        houseNumber.isValid should be(false)
        val error = ParseError("house_number")
        houseNumber == Validated.invalid(error) should be(true)
    }

}

