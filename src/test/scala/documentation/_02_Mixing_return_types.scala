package documentation

import org.qirx.littlespec.Specification
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._

object _02_Mixing_return_types extends Specification {

  """|# Mixing return types
     |
     |You migh have noticed that for comprehensions become tricky to use if 
     |you want to use them for different container types. Most people rely on 
     |the concept of monad transformers to deal with this.
     |
     |This library gives you a set of tools that (in my opinion) simplify the
     |proces of straightening out the types. When working with monad transformers
     |you need to think about the resulting (complex) type when working on the 
     |structure of the program. These tools allow you to delay that until you want
     |to get the actual result.
     |
     |Note that it is required to define a monadic instance for the result.""".stripMargin - example {
    import org.qirx.programbuilder.Implicits._
    import org.qirx.programbuilder.~>
    import org.qirx.programbuilder.Monadic
    import scala.concurrent.ExecutionContext.Implicits.global

    // The parts that make up the program
    object ProgramParts {
      trait CustomPart[T]
      case class Get(id: String) extends CustomPart[String]
      case class Save(value: String) extends CustomPart[Unit]
      case class Convert(value: String) extends CustomPart[String]
    }

    import ProgramParts._

    // The program itself
    def program(id: String) =
      for {
        value <- Get(id)
        convertedValue <- Convert(value)
        _ <- Save(convertedValue)
      } yield ()

      // The program runner
    type FutureOption[A] = Future[Option[A]]
    object ProgramRunner extends (CustomPart ~> FutureOption) {
      def transform[x] = {
        case Get(id) =>
          val result = if (id == "test") Some("value") else None
          Future successful result
        case Save(value) =>
          val saved = Future successful (())
          Future successful Some(())
        case Convert(value) =>
          val result = value.toUpperCase
          Future successful Some(result)
      }
    }

    // Running the program
    implicit def monadic(implicit ec: ExecutionContext) =
      new Monadic[FutureOption] {
        def create[A](a: A) = Future successful Option(a)
        def flatMap[A, B](fa: FutureOption[A])(f: A => FutureOption[B]) =
          fa.flatMap(_ map f getOrElse (Future successful None))
      }

    val result1 = program("test") runWith ProgramRunner
    val result2 = program("foo") runWith ProgramRunner

    Await.result(result1, 1.second) is Some(())
    Await.result(result2, 1.second) is None
  }

  """|Introducing a program with multiple parts.
     |
     |In the above example the program is clean and clear of type straightening. 
     |The straigthening is however poluting the actual execution. We can separate 
     |those as well using a program that consists of multiple parts.
     |
     |As you can see this involves more code. The upside is that the code is easier
     |to understand and compose. As your project grows more complex, the complexity 
     |of the code itself remains stable.
     | 
     |Note that it is required to define a monadic instance for the result type.""".stripMargin - example {
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

    // The program itself
    def program(id: String) = {
      implicit val programType = ProgramType[Index +: Store +: Util +: Nil]

      for {
        value <- Get(id)
        convertedValue <- Convert(value)
        _ <- Save(convertedValue)
      } yield ()
    }

    // The program runners
    type FutureOption[A] = Future[Option[A]]
    object IndexRunner extends (Index ~> FutureOption) {
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
        case Convert(value) => value.toUpperCase
      }
    }

    object FutureToFutureOption extends (Future ~> FutureOption) {
      def transform[x] = _ map Option.apply
    }
    
    object IdToFuture extends (Id ~> Future) {
      def transform[x] = Future successful _
    }
    
    val indexRunner = IndexRunner
    val storeRunner = StoreRunner andThen FutureToFutureOption
    val utilRunner = UtilRunner andThen IdToFuture andThen FutureToFutureOption
    
    val programRunner = indexRunner +: storeRunner +: utilRunner

    // Running the program
    implicit def monadic(implicit ec: ExecutionContext) =
      new Monadic[FutureOption] {
        def create[A](a: A) = Future successful Option(a)
        def flatMap[A, B](fa: FutureOption[A])(f: A => FutureOption[B]) =
          fa.flatMap(_ map f getOrElse (Future successful None))
      }
    
    val result1 = program("test") runWith programRunner
    val result2 = program("foo") runWith programRunner

    Await.result(result1, 1.second) is Some(())
    Await.result(result2, 1.second) is None
  }
}