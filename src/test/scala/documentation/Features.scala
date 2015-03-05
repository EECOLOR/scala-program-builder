package documentation

import org.qirx.littlespec.Specification
import scala.concurrent.Future

object Features extends Specification {

  """|#Simple programs
     |
     |- Implicit conversion of any `_[_]` shaped type to a program
     |- `Static[ReturnType]` for elements that already have a value
     |- `ValueOf(value)` constructor for known values
     |- `run` method on programs that simply produces the value of the program""".stripMargin - example {
    import org.qirx.programbuilder._

    case class StaticPart(input: String) extends Static[Boolean] {
      def result = input == "test"
    }

    val program =
      for {
        value <- ValueOf("test")
        result <- StaticPart(value)
      } yield result

    program.run is true
  }

  """|#Custom programs
     |
     |Separating definition from implementation 
     |
     |- A `runWith` method that allows you to specify a runner
     |- A weird arrow `~>` (called natural transformation) representing a runner
     |- The `Id[x]` type""".stripMargin - example {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart[ReturnType]
      case class Part1(value: String) extends CustomPart[String]
      case class Part2(value: String) extends CustomPart[Boolean]
    }

    import Parts._

    val program =
      for {
        value <- Part1("test")
        result <- Part2(value)
      } yield result

    val runner =
      new (CustomPart ~> Id) {
        def transform[x] = {
          case Part1(value) => value + value
          case Part2(value) => value == "testtest"
        }
      }

    program runWith runner is true
  }

  """|#Complex types as a result
     |
     |Hiding type complexity from program definition
     |
     |- The `Monadic` trait to help transform your program to some real world types
     |""".stripMargin - example {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart[ReturnType]
      case class Part1(value: String) extends CustomPart[String]
      case class Part2(value: String) extends CustomPart[Boolean]
    }

    import Parts._

    val program =
      for {
        value <- Part1("test")
        result <- Part2(value)
      } yield result

    type ResultType[x] = Option[Either[String, x]]

    val runner =
      new (CustomPart ~> ResultType) {
        def transform[x] = {

          case Part1(value) =>
            val result =
              if (value == "test") Right(value + value)
              else Left("Could not compute value")
            Option(result)

          case Part2(value) =>
            Option(Right(value == "testtest"))
        }
      }

    implicit val monadic =
      new Monadic[ResultType] {
        def create[A](a: A) = Option(Right(a))

        def flatMap[A, B](fa: ResultType[A])(f: A => ResultType[B]) =
          fa.flatMap {
            case Left(string) => Option(Left(string))
            case Right(a)     => f(a)
          }
      }

    program runWith runner is Some(Right(true))
  }

  """|#Multipart programs
     |
     |Helps with grouping program operations
     |
     |- Implicit conversion of a part to a program consisting of multiple groups
     |- `ProgramType` class to provide the type of the program
     |- `::` and `Nil` type operators to clearly specify the groups
     |- `or` method on runners to combine them
     |- Type alignment of program type and runner
     |- Monadic instance for `Id`""".stripMargin - example {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart1[ReturnType]
      case class Part1(value: String) extends CustomPart1[String]
      trait CustomPart2[ReturnType]
      case class Part2(value: String) extends CustomPart2[Boolean]
    }

    import Parts._

    implicit val programType = ProgramType[CustomPart1 :+: CustomPart2 :+: CNil]

    val program =
      for {
        value <- Part1("test")
        result <- Part2(value)
      } yield result

    val customPart1Runner =
      new (CustomPart1 ~> Id) {
        def transform[x] = {
          case Part1(value) => value + value
        }
      }

    val customPart2Runner =
      new (CustomPart2 ~> Id) {
        def transform[x] = {
          case Part2(value) => value == "testtest"
        }
      }

    val runner = customPart1Runner :+: customPart2Runner

    program runWith runner is true
  }

  """|#Type differences
     |
     |Hiding type differences of groups from the implementation
     |
     |- `andThen` method on runners to compose them
     |- Monadic instance for `Either[L, x]` (right biased)""".stripMargin - example {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart1[ReturnType]
      case class Part1(value: String) extends CustomPart1[String]
      trait CustomPart2[ReturnType]
      case class Part2(value: String) extends CustomPart2[Boolean]
    }

    import Parts._

    implicit val programType = ProgramType[CustomPart1 :+: CustomPart2 :+: CNil]

    val program =
      for {
        value <- Part1("test")
        result <- Part2(value)
      } yield result

    type ResultType[x] = Either[String, x]

    val customPart1Runner =
      new (CustomPart1 ~> ResultType) {
        def transform[x] = {
          case Part1(value) =>
            if (value == "test") Right(value + value)
            else Left(s"Could not process value '$value'")
        }
      }

    val customPart2Runner =
      new (CustomPart2 ~> Id) {
        def transform[x] = {
          case Part2(value) => value == "testtest"
        }
      }

    val idToResultType =
      new (Id ~> ResultType) {
        def transform[x] = Right apply _
      }

    val runner = customPart1Runner :+: (customPart2Runner andThen idToResultType)

    program runWith runner is Right(true)
  }

  """|#Branching
     |
     |Allow programs to branch
     |
     |- `ifEmpty` enhancement for parts that return an `Option`
     |- `mergeBranch` method that merges a branch if the type is the same as 
     |  the result of the non-branched part""".stripMargin - {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart[ReturnType]
      case object Part1 extends CustomPart[Option[String]]
      case class Part2(value: String) extends CustomPart[Boolean]
    }

    import Parts._

    implicit val programType = ProgramType[CustomPart :+: Static :+: CNil]
      .withBranch[Boolean]

    val program =
      for {
        value <- Part1 ifNone Return(false)
        _ <- Part2(value) ifFalse
          Return(false)
      } yield true

    type ResultType[x] = Either[String, x]

    val customPartRunner =
      new (CustomPart ~> Id) {
        def transform[x] = {

          case Part1        => Some("testtest")

          case Part2(value) => value == "testtest"
        }
      }

    val runner = customPartRunner :+: Static.Runner

    program.mergeBranch runWith runner is true
  }

  """|#Nested programs
     |
     |Allows the usage of programs as program parts
     |
     |- `toPrgramType` method that can be used to adjust the program type
     |- `adjustType` method to convert the program to a compatible type
     |- `autoAdjust` method to convert the runner to the correct type
     |""".stripMargin - example {
    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart1[ReturnType]
      case class Part1(value: String) extends CustomPart1[String]
      trait CustomPart2[ReturnType]
      case class Part2(value: String) extends CustomPart2[Boolean]
      trait CustomPart3[ReturnType]
      case class Part3(value: String) extends CustomPart3[Int]
    }

    import Parts._

    val subProgram = {
      implicit val programType = ProgramType[CustomPart1 :+: CustomPart2 :+: CNil]

      for {
        value <- Part1("test")
        result <- Part2(value)
      } yield result
    }

    val program = {
      implicit val programType =
        ProgramType[CustomPart1 :+: CustomPart2 :+: CustomPart3 :+: CNil]

      for {
        value1 <- subProgram.toProgramType
        resul2 <- Part3(value1.toString)
        result <- subProgram.adjustType[programType.Out]
      } yield result
    }

    val customPart1Runner =
      new (CustomPart1 ~> Id) {
        def transform[x] = {
          case Part1(value) => value + value
        }
      }

    val customPart2Runner =
      new (CustomPart2 ~> Id) {
        def transform[x] = {
          case Part2(value) => value == "testtest"
        }
      }

    val customPart3Runner =
      new (CustomPart3 ~> Id) {
        def transform[x] = {
          case Part3(value) => 1
        }
      }

    val runner = customPart3Runner :+: customPart2Runner :+: customPart1Runner

    program runWith runner.autoAdjust is true
  }

  """|#Parts with map or flatMap functions
     |
     |Allows the usage of parts that will not implicitly convert
     |
     |- Runner to convert an option to a sequence
     |- Monadic instance for Seq
     |""".stripMargin - example {
    import org.qirx.programbuilder._

    implicit val programType = ProgramType[Option :+: List :+: CNil]

    val program =
      for {
        value <- Option("test").toProgram
        result <- List(value).toProgram
      } yield result

    val optionRunner = implicitly[Option ~> Seq]
    val listRunner = implicitly[List ~> Seq]

    val runner = optionRunner :+: listRunner

    program runWith runner is Seq("test")
  }

  """|#Unapply in a for comprehension
     |
     |document""".stripMargin - example {
    import org.qirx.programbuilder._

    implicit val programType = ProgramType[List :+: Option :+: CNil]

    val program =
      for {
        (value1, value2) <- Option("test" -> "test").toProgram
        result <- List(value1 + value2).toProgram
      } yield result

    val optionRunner = implicitly[Option ~> Seq]
    val listRunner = implicitly[List ~> Seq]

    val runner = listRunner :+: optionRunner

    program runWith runner is Seq("testtest")
  }

  """|#Special program type
     |
     |Improves implicit resolution for larger programs""".stripMargin - {

    import org.qirx.programbuilder._

    object Parts {
      trait CustomPart1[ReturnType]
      case object Part1 extends CustomPart1[Option[String]]
      trait CustomPart2[ReturnType]
      case class Part2(value: String) extends CustomPart2[Boolean]
    }

    import Parts._

    import scala.language.higherKinds

    implicit val programType = ProgramType[CustomPart1 :+: CustomPart2 :+: Static :+: CNil]
      .withBranch[Boolean]

    def program[O[_]](
      implicit programType: O With (CustomPart1 :+: CustomPart2 :+: Static :+: CNil) WithBranch Boolean) = {

      import programType.injector

      for {
        value <- Part1 ifNone Return(false)
        _ <- Part2(value) ifFalse Return(false)
      } yield true
    }

    type ResultType[x] = Either[String, x]

    val customPart1Runner =
      new (CustomPart1 ~> Id) {
        def transform[x] = {
          case Part1        => Some("testtest")
        }
      }

    val customPart2Runner =
      new (CustomPart2 ~> Id) {
        def transform[x] = {
          case Part2(value) => value == "testtest"
      }
    }

    val runner = customPart1Runner :+: customPart2Runner :+: Static.Runner

    program.mergeBranch runWith runner is true
  }
}