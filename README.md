# Installation

Coming, for now, just clone and build

# The library

This library aims to provide the building blocks to create readable code.

A program:
``` scala
implicit val programType = ProgramType[Store :: Static :: Nil].withBranch[Result]

val program
  for {
    existingDocument <- GetDocumentById(id) ifEmpty Return(NotFound)
    json <- JsonFromRequest(request) ifEmpty Return(BadRequest)
    newDocument <- JsObjectFromJsValue(json) ifEmpty Return(UnprocessableEnity)
    mergedDocument <- MergeDocuments(existingDocument, newDocument)
    _ <- SaveDocument(id, mergedDocument)
  } yield NoContent
```

In my opinion programs should look like this. It clearly communicates the intention 
and the different steps.

To make it compile we need the different parts that the program is composed of.

The parts:
``` scala
sealed trait Store[T]
case class GetDocumentById(id:String) extends Store[Option[JsValue]]
case class SaveDocument(id:String, document:Json) extends Store[JsValue]

case class JsonFromRequest(request:Request) extends Static[Option[JsValue]] {
  def result = request.body.asJson
}
case class JsObjectFromJsValue(json:JsValue) extends Static[Option[JsObject]] {
  def result = json.asOpt[JsObject]
}
case class MergeDocuments(doc1:JsObject, doc2:JsObject) extends Static[JsObject] {
  def result = doc1 ++ doc2
} 
```

Parts are like methods. Defining them like this allows you to separate the body from 
the arguments. 

A runner:
``` scala
val storeRunner = 
  new (Store ~> Future) {
    def transform[x] = {
      case GetDocumentById(id) => 
        val result:Future[Option[JsObject]] = ...
        result
      case SaveDcoument(id, doc) =>
        val result:Future[Unit] = ...
        result
    }
  }
  
val staticRunner = Static.runner andThen implicitly[Id ~> Future]
  
val runner = storeRunner or staticRunner
```

Running different parts of the code differently allows you to think about what is 
actually important. Aligning types can be done independently of the implementation 
of the method. 

Running the program:
``` scala
program.mergeBranch runWith runner
```

Merging a branch before running the program saves you from dealing with complex 
`Either` types throughout the program.

Check the documentation directory for an explanation of the provided features.

# Why this library?

It started with a presentation called [Compositional application architecture with reasonably priced monads](https://parleys.com/play/53a7d2c3e4b0543940d9e538) 
by Rúnar Bjarnason ([the slides](https://t.co/QsBDMDqGGE) and [the code](https://gist.github.com/runarorama/a8fab38e473fafa0921d)).

This presentation sparked my interest led to experiment with different things. One 
of the experiments was with [type inferred programs](https://gist.github.com/EECOLOR/425ce70af05bf79781c2) that
would determine their type based the parts being used. I eventually came to the 
conclusion that this is actually less usable than specifying the type beforehand.

To explore the subject a bit more and also look at the relation to monad transformers 
I created [a repository](https://github.com/EECOLOR/scala-clean-code-patterns/blob/master/src/main/scala/processes/README.md).

I have also explored the use of ScalaZ as the base for this library. It however has
a few problems when it comes to type inference as explained [in this issue](https://github.com/scalaz/scalaz/issues/827).

On top of that ScalaZ is a scary library because it contains a lot of stuff. Every 
time I ventured into the source code I got lost. Improving on this seems very 
difficult and I have no clear suggestions. One of the major problems complicating 
the understandability of ScalaZ and functional programming in general is the 
abstractness of things. 

What would be meaningful names for methods with the following signatures?

- `A => F[A]`
- `F[A] => (A => B) => F[B]`
- `F[A] => (A => F[B]) => F[B]`
- `F[F[A]] => F[A]`

Would those names still be meaningful in all places where you use them?

Anyway, in this library I utilize a lot of concepts from functional programming and
even used names that are used in ScalaZ as well. It however implements a minimal 
amount of code with as biggest focus: readability and easy of use.

Instead of `Free` I am using the name `Program` and instead of `Monad` I use the 
name `Monadic`, it does not implement all the methods you would expect from a 
`Monad`.



 
 
 