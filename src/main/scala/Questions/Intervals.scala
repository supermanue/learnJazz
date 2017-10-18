package Questions

import Harmony._
import Util.util

import scala.util.Random

object Intervals extends QuestionGroup {


  def ascendingInterval(q: Question):(String, String)={
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote + interval

    val question="intervalo de " + interval + " ascendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }

  def descendingInterval(q: Question):(String, String)={
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote - interval

    val question="intervalo de " + interval + " descendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case ascendingInterval => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))
    case descendingInterval => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))
  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(ascendingInterval), ascendingInterval),
    (questionGenerator(descendingInterval), descendingInterval)))



}
