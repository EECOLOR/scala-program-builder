package documentation

import org.qirx.littlespec.Specification
import scala.concurrent.Future

object _01_Basic_program extends Specification {

  """|# A basic program
     |
     |This example is not a very good argument to convince you to use this style 
     |of programming. It however clearly shows the separation of building blocks,
     |structure and execution of the program.
     |
     |This separation of concerns will (with more complex examples) make more 
     |sense.""".stripMargin - example {
    import org.qirx.programbuilder._

    // The parts that make up the program
    object ProgramParts {
      trait CustomPart[ReturnType]
      case class Append(input1: String, input2: String) extends CustomPart[String]
      case class Store(value: String) extends CustomPart[Boolean]
    }

    import ProgramParts._

    // The program itself
    def program(input1: String, input2: String) =
      for {
        appended <- toProgram(Append(input1, input2))
        stored <- Store(appended)
      } yield stored

    // The program runner
    object ProgramRunner extends (CustomPart ~> Id) {
      def transform[x] = {
        case Append(input1, input2) => input1 + "-" + input2
        case Store(value)           => value == "test-test"
      }
    }

    // Running the program
    program("a", "b") runWith ProgramRunner is false
    program("test", "test") runWith ProgramRunner is true
  }

  """|The above example shows the manual version of a transformation of building 
     |blocks to the `Id` container. Note that `Id` is expressed as `Id[A] = A`. 
     |A nifty trickthat allows us to treat any type as a container type, even if it 
     |is not one.
     |
     |If you have program parts that can be executed directly, or for which the value 
     |is already calculated, you can use some types made available by the library.
     |
     |Again, just as with the previous example, it seems more complicated than needed. 
     |I am slowly introducing the different building blocks this library makes 
     |available. The building blocks will prove their value as the types that are 
     |returned by methods become more complex.""".stripMargin - example {
    import org.qirx.programbuilder._

    // The parts that make up the program
    object ProgramParts {
      case class Store(value: String) extends Static[Boolean] {
        def result = value == "test-test"
      }
    }

    import ProgramParts._

    // The program itself
    def program(input1: String, input2: String) =
      for {
        appended <- Return(input1 + "-" + input2)
        stored <- Store(appended)
      } yield stored

    val program1 = program("a", "b")
    val program2 = program("test", "test")

    // running the program
    program1.run is false
    program2.run is true
  }

}