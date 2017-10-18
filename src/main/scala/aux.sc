import Harmony._
import Questions._
import User._



val myUser = new User(0, "manue", List[QuestionGroup]())

myUser.addQuestionArea(Triads)
myUser.addQuestionArea(Dominants)


var x = 1

for ( x <- 1 until 10){
  println("value of x: " + x)
  val question = myUser.generateQuestion()
  val answer = question._2(question._1)
  println(answer._1)
  scala.io.StdIn.readLine()
  println(answer._2)
}

println("end")