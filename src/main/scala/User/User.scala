package User

import Questions.{Question, QuestionGroup}
import Util._

class User (val id: Int, val username: String, var questions: List[QuestionGroup]) {

  def addQuestionArea(q: QuestionGroup) =
    if (!questions.contains(q))
      this.questions = questions :+ q

  def deleteQuestionArea(q: QuestionGroup) =
    if (questions.contains(q))
      this.questions = questions.diff(Seq(q))


  def generateQuestion():(Question, Question => (String, String)) =
    util.randomElement(questions).randomQuestion()


}
