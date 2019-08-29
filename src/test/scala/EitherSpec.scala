import cats.implicits._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-29.
  */
class EitherSpec extends FunSuite with Matchers {

    test("is right biased") {
        val right: Either[String, Int] = Either.right(5)
        right.map(_ + 1) should be(Either.right(6))

        val left: Either[String, Int] = Either.left("Something went wrong")
        left.map(_ + 1) should be(Either.left("Something went wrong"))
    }

    test("has a Monad implementation") {
        val right: Either[String, Int] = Either.right(5)
        right.flatMap(x => Either.right(x + 1)) should be(Either.right(6))

        val left: Either[String, Int] = Either.left("Something went wrong")
        left.flatMap(x => Either.right(x + 1)) should be(Either.left("Something went wrong"))
    }

    object EitherStyle {
        def parse(s: String): Either[NumberFormatException, Int] =
            if (s.matches("-?[0-9]+")) Either.right(s.toInt)
            else Either.left(new NumberFormatException(s"$s is not a valid integer."))

        def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
            if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
            else Either.right(1.0 / i)

        def stringify(d: Double): String = d.toString

        def magic(s: String): Either[Exception, String] =
            parse(s).flatMap(reciprocal).map(stringify)
    }

    test("Either instead of exceptions") {
        EitherStyle.parse("Not a number").isRight should be(false)
        EitherStyle.parse("2").isRight should be(true)
    }

    test("Either composes nicely") {
        import EitherStyle._
        magic("0").isRight should be(false)
        magic("1").isRight should be(true)
        magic("Not a number").isRight should be(false)
    }

    test("Either can carry exceptions on the left") {
        import EitherStyle._
        val result = magic("2") match {
            case Left(_: NumberFormatException) => "Not a number!"
            case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
            case Left(_) => "Unknown error"
            case Right(result) => s"Got reciprocal: $result"
        }
        result should be("Got reciprocal: 0.5")
    }

    object EitherStyleWithAdts {
        sealed abstract class Error
        final case class NotANumber(string: String) extends Error
        final case object NoZeroReciprocal extends Error

        def parse(s: String): Either[Error, Int] =
            if (s.matches("-?[0-9]+")) Either.right(s.toInt)
            else Either.left(NotANumber(s))

        def reciprocal(i: Int): Either[Error, Double] =
            if (i == 0) Either.left(NoZeroReciprocal)
            else Either.right(1.0 / i)

        def stringify(d: Double): String = d.toString

        def magic(s: String): Either[Error, String] =
            parse(s).flatMap(reciprocal).map(stringify)
    }

    test("Either can carry a value of an error ADT on the left") {
        import EitherStyleWithAdts._

        val result = magic("2") match {
            case Left(NotANumber(_)) => "Not a number!"
            case Left(NoZeroReciprocal) => "Can't take reciprocal of 0!"
            case Right(result) => s"Got reciprocal: $result"
        }
        result should be("Got reciprocal: 0.5")
    }

//    sealed abstract class DatabaseError
//    trait DatabaseValue
//
//    object Database {
//        def databaseThings(): Either[DatabaseError, DatabaseValue] = ???
//    }
//
//    sealed abstract class ServiceError
    trait ServiceValue
//
//    object Service {
//        def serviceThings(v: DatabaseValue): Either[ServiceError, ServiceValue] = ???
//    }

    // This doesn't work!
//    def doApp = Database.databaseThings().flatMap(Service.serviceThings)


//    sealed abstract class AppError
//    final case object DatabaseError1 extends AppError
//    final case object DatabaseError2 extends AppError
//    final case object ServiceError1 extends AppError
//    final case object ServiceError2 extends AppError
//
//    trait DatabaseValue
//
//    object Database {
//        def databaseThings(): Either[AppError, DatabaseValue] = ???
//    }
//
//    object Service {
//        def serviceThings(v: DatabaseValue): Either[AppError, ServiceValue] = ???
//    }
//
//    def doApp: Either[AppError, ServiceValue] = Database.databaseThings().flatMap(Service.serviceThings)


    sealed abstract class DatabaseError
    trait DatabaseValue

    object Database {
        def databaseThings(): Either[DatabaseError, DatabaseValue] = ???
    }

    sealed abstract class ServiceError
    trait Service

    object Service {
        def serviceThings(v: DatabaseValue): Either[ServiceError, ServiceValue] = ???
    }

    sealed abstract class AppError
    object AppError {
        final case class Database(error: DatabaseError) extends AppError
        final case class Service(error: ServiceError) extends AppError
    }

    def doApp: Either[AppError, ServiceValue] =
        Database.databaseThings()
            .leftMap(AppError.Database)
            .flatMap(dv => Service.serviceThings(dv).leftMap(AppError.Service))

    def awesome: String = doApp match {
        case Left(AppError.Database(_)) => "something in the database went wrong"
        case Left(AppError.Service(_)) => "something in the service went wrong"
        case Right(_) => "everything is alright!"
    }

    test("Either in the large") {
        val right: Either[String, Int] = Right(41)
        right.map(_ + 1) should be(Right(42))

        val left: Either[String, Int] = Left("Hello")
        left.map(_ + 1) should be(Left("Hello"))
        left.leftMap(_.reverse) should be(Left("olleH"))
    }


    val either: Either[NumberFormatException, Int] = try {
        Either.right("abc".toInt)
    } catch {
        case nfe: NumberFormatException => Either.left(nfe)
    }

    val either2: Either[NumberFormatException, Int] =
        Either.catchOnly[NumberFormatException]("abc".toInt)

    test("Either with exceptions") {
        Either.catchOnly[NumberFormatException]("abc".toInt).isRight should be(false)
        Either.catchNonFatal(1 / 0).isLeft should be(true)
    }


    val right: Either[String, Int] = 7.asRight[String]
    val left: Either[String, Int] = "hello 🐈s".asLeft[Int]

    test("Either syntax") {
        val right: Either[String, Int] = 42.asRight[String]
        right should be(Either.right[String, Int](42))
    }

}
