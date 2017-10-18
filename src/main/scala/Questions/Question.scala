package Questions

import Harmony.Note


class Question(val rootNote: Note, val number: Int) {
}

trait QuestionGroup{
  def questionGenerator (f: Question=>(String, String)): Question
  def randomQuestion(): (Question, Question=>(String, String))

}