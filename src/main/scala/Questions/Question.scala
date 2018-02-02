package Questions

import Harmony.Note

case class QuestionArguments(rootNote: Note, number: Int) {
}

trait Question {
  def apply(q: QuestionArguments): (String, String)
  def randomArguments(): QuestionArguments
 }

trait QuestionGroup{
  def randomQuestion(): Question
}
