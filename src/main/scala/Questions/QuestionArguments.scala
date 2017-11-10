package Questions

import Harmony.Note

case class QuestionArguments(rootNote: Note, number: Int) {
}

case class Question(questionArguments: QuestionArguments,
                    questionFunction: QuestionArguments=>(String, String))

trait QuestionGroup{
  def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments
  def randomQuestion(): Question
}
