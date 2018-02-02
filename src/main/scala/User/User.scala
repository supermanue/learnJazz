package User

import Questions.{Question, QuestionGroup}
import Util._

case class User (val username: String, var questionGroups: List[QuestionGroup]) {

  def addQuestionArea(q: QuestionGroup):Unit =
    if (!questionGroups.contains(q))
      this.questionGroups = questionGroups :+ q

  def deleteQuestionArea(q: QuestionGroup):Unit =
    if (questionGroups.contains(q))
      this.questionGroups = questionGroups.diff(Seq(q))


  def generateQuestion(): Question =
    util.randomElement(questionGroups).randomQuestion()

  override def toString: String = username + ". Questions areas: " + questionGroups.mkString(";")
}
