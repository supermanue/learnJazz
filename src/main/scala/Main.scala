import Questions.{Dominants, QuestionGroup, Triads}
import User.User

object Main extends App{

  val myUser = new User(0, "manue", List[QuestionGroup]())

  myUser.addQuestionArea(Triads)
  myUser.addQuestionArea(Dominants)


  var x = 1

  for ( x <- 1 until 0){
    println("value of x: " + x)
    val question = myUser.generateQuestion()
    val (questionText, answerText) = question.questionFunction(question.questionArguments)

    println(questionText)
    println("Pulsa intro para ver la respuesta")
    scala.io.StdIn.readLine()
    println(answerText)
    println ("Acertaste? y/n")
    val input=scala.io.StdIn.readLine()
    if (input=="y")
      println("") //TODO: do something here, like store the answer or whatever
    else
      println("")

  }


}
