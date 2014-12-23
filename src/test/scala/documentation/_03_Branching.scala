package documentation

import org.qirx.littlespec.Specification
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.higherKinds

object _03_Branching extends Specification {

  """|#Branching
     |
     |Most programs require some form of branching. This library facilitates 
     |that in two ways. The first is by hiding the branching process. The 
     |second is by making it an obvious part of the program.
     |
     |This example hides the branching.
     |
     |Note that we end up with a program that has an `Either` as part of it's 
     |result. We merge it after running the program.""".stripMargin - example {
    import org.qirx.programbuilder.Implicits._
    import org.qirx.programbuilder.~>
    import org.qirx.programbuilder.Id
    import org.qirx.programbuilder.Monadic
    import org.qirx.programbuilder.ProgramType
    import org.qirx.programbuilder.+:
    import org.qirx.programbuilder.Nil
    import scala.concurrent.ExecutionContext.Implicits.global

    // The parts that make up the program
    object ProgramParts {
      trait Index[T]
      case class Get(id: String) extends Index[String]
      trait Store[T]
      case class Save(value: String) extends Store[Unit]
      trait Util[T]
      case class Convert(value: String) extends Util[String]
    }

    import ProgramParts._

    sealed trait Result
    object Success extends Result
    object Failure extends Result

    // The program itself
    def program(id: String) = {
      implicit val programType = ProgramType[Index +: Store +: Util +: Nil]

      for {
        value <- Get(id)
        convertedValue <- Convert(value)
        _ <- Save(convertedValue)
      } yield Success
    }

    // The program runners
    type FutureResult[A] = Future[Either[Result, A]]
    object IndexRunner extends (Index ~> FutureResult) {
      def transform[x] = {
        case Get(id) =>
          val result = if (id == "test") Some("value") else None
          val converted = result.toRight(Failure)
          Future successful converted
      }
    }

    object StoreRunner extends (Store ~> Future) {
      def transform[x] = {
        case Save(value) => Future successful (())
      }
    }

    object UtilRunner extends (Util ~> Id) {
      def transform[x] = {
        case Convert(value) => value.toUpperCase
      }
    }

    object FutureToFutureResult extends (Future ~> FutureResult) {
      def transform[x] = _ map Right.apply recover {
        case _: Throwable => Left(Failure)
      }
    }

    object IdToFuture extends (Id ~> Future) {
      def transform[x] = Future successful _
    }

    val indexRunner = IndexRunner
    val storeRunner = StoreRunner andThen FutureToFutureResult
    val utilRunner = UtilRunner andThen IdToFuture andThen FutureToFutureResult

    val programRunner = indexRunner +: storeRunner +: utilRunner

    // Running the program
    implicit def monadic(implicit ec: ExecutionContext) =
      new Monadic[FutureResult] {
        def create[A](a: A) = Future successful Right(a)
        def flatMap[A, B](fa: FutureResult[A])(f: A => FutureResult[B]) =
          fa.flatMap {
            case Left(value)  => Future successful Left(value)
            case Right(value) => f(value)
          }
      }

    val result1 = program("test") runWith programRunner map (_.merge)
    val result2 = program("foo") runWith programRunner map (_.merge)

    Await.result(result1, 1.second) is Success
    Await.result(result2, 1.second) is Failure
  }

  """|#Introducing branching
     |
     |In this example we introduce the concept of a branch. This allows us to 
     |merge the branch in our program before we run it.
     |
     |Note that we are using a slightly different return type for the `Get`
     |method. We could use a similar style while merging manually, this version 
     |however shows some features made available by the library.
     |
     |Using this style simplifies running the program.
     |""".stripMargin - example {
    import org.qirx.programbuilder.Implicits._
    import org.qirx.programbuilder.~>
    import org.qirx.programbuilder.Id
    import org.qirx.programbuilder.Monadic
    import org.qirx.programbuilder.ProgramType
    import org.qirx.programbuilder.+:
    import org.qirx.programbuilder.Nil
    import org.qirx.programbuilder.Brancher
    import org.qirx.programbuilder.Branch
    import org.qirx.programbuilder.Method
    import org.qirx.programbuilder.Return
    import scala.concurrent.ExecutionContext.Implicits.global

    // The parts that make up the program
    object ProgramParts {
      trait Index[T]
      case class Get(id: String) extends Index[Option[String]]
      trait Store[T]
      case class Save(value: String) extends Store[Unit]
      trait Util[T]
      case class Convert(value: String) extends Util[List[String]]
    }

    implicit class OptionMethodEnhancements[F[_], R](right: F[Option[R]]) {

      def ifEmpty[G[_], L](left: G[L])(
        implicit brancher: Brancher[F, G, L]) =

        // When the option is empty, use the result of left
        brancher.branch(right)(_.toRight(left))
    }
    
    implicit class ListMethodEnhancements[F[_], R](right: F[List[R]]) {
    	
    	def ifEmpty[G[_], L](left: G[L])(
    			implicit brancher: Brancher[F, G, L]) =
    			
    			// When the option is empty, use the result of left
    			brancher.branch(right)(list => if (list.isEmpty) Left(left) else Right(list))
    }

    import ProgramParts._

    sealed trait Result
    val Success = new Result {}
    val Failure = new Result {}

    // The program itself
    implicit val programType = 
    ProgramType[Index +: Store +: Util +: Method +: Branch[Result]#Instance +: Nil]
    
    def program(id: String) = {

      for {
        value <- Get(id) ifEmpty Return(Failure)
        convertedValue <- Convert(value) ifEmpty Return(Failure)
        _ <- Save(convertedValue.mkString)
      } yield Success
    }

    // The program runners
    object IndexRunner extends (Index ~> Future) {
      def transform[x] = {
        case Get(id) =>
          val result = if (id == "test") Some("value") else None
          Future successful result
      }
    }

    object StoreRunner extends (Store ~> Future) {
      def transform[x] = {
        case Save(value) => Future successful (())
      }
    }

    object UtilRunner extends (Util ~> Id) {
      def transform[x] = {
        case Convert(value) => value.toList.map(_.toString)
      }
    }

    object IdToFuture extends (Id ~> Future) {
      def transform[x] = Future successful _
    }

    val indexRunner = IndexRunner
    val storeRunner = StoreRunner
    val utilRunner = UtilRunner andThen IdToFuture
    val methodRunner = Method.MethodRunner andThen IdToFuture
    
    val programRunner = indexRunner +: storeRunner +: utilRunner +: methodRunner

    // Running the program
    implicit def monadic(implicit ec: ExecutionContext) =
      new Monadic[Future] {
        def create[A](a: A) = Future successful a
        def flatMap[A, B](fa: Future[A])(f: A => Future[B]) =
          fa flatMap f
      }

    val result1 = program("test").mergeBranch runWith programRunner
    val result2 = program("foo").mergeBranch runWith programRunner
    val result3 = program("").mergeBranch runWith programRunner

    Await.result(result1, 1.second) is Success
    Await.result(result2, 1.second) is Failure
    Await.result(result3, 1.second) is Failure
  }

     "program runners should not match program type in the same order" - {}
     
}