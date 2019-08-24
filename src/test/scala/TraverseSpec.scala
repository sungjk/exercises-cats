import cats.Applicative
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeremy on 2019-08-22.
  */
class TraverseSpec extends FunSuite with Matchers {
    import scala.concurrent.Future

    def parseInt(s: String): Option[Int] = ???

    trait SecurityError
    trait Credentials

    def validateLogin(cred: Credentials): Either[SecurityError, Unit] = ???


    trait Profile
    trait User

    def userInfo(user: User): Future[Profile] = ???
    def profilesFor(users: List[User]): List[Future[Profile]] = users.map(userInfo)

    // F: List
    // G: Option, Either, Future
    // traverse: List[Future[Profile]] => Future[List[Profile]]
    trait Traverse[F[_]] {
        def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    }


    import cats.data.{Validated, ValidatedNel}
    import cats.implicits._

    def parseIntEither(s: String): Either[NumberFormatException, Int] =
        Either.catchOnly[NumberFormatException](s.toInt)

    def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
        Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel


    test("traverseU function with either") {
        import cats.implicits._
//        List("1", "2", "3").traverse(parseIntValidated) should be(Right(List(1, 2, 3)))
        List("1", "abc", "3").traverse(parseIntEither).isLeft should be(true)
    }

    test("traverseU function with validated") {
        List("1", "2", "3").traverse(parseIntValidated).isValid should be(true)
    }


    import cats.data.Reader

    trait Context
    trait Topic
    trait Result

    type Job[A] = Reader[Context, A]

    def processTopic(topic: Topic): Job[Result] = ???
    def processTopics(topics: List[Topic]): Job[List[Result]] = topics.traverse(processTopic)


    test("sequencing effects") {
        List(Option(1), Option(2), Option(3)).traverse(identity) should be(Some(List(1, 2, 3)))
        List(Option(1), None, Option(3)).traverse(identity) should be(None)
    }


    trait Data
    def writeToStore(data: Data): Future[Unit] = ???

    import cats.implicits._

    import scala.concurrent.ExecutionContext.Implicits.global

    def writeManyToStore(data: List[Data]): Future[List[Unit]] = data.traverse(writeToStore)


    test("traversing for effects") {
        List(Option(1), Option(2), Option(3)).sequence_ should be(Some())
        List(Option(1), None, Option(3)).sequence_ should be(None)
    }
}
