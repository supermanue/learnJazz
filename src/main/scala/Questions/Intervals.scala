package Questions

import Harmony._
import Util.util

import scala.util.Random



object ascendingInterval extends Question{

  override def toString: String = "ascendingInterval"
  def apply(q:QuestionArguments):(String, String) = {
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote + interval
    val question="intervalo de " + interval + " ascendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }

  def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))

}

object descendingInterval extends Question{
  override def toString: String = "descendingInterval"
  def apply(q:QuestionArguments):(String, String) = {
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote - interval
    val question="intervalo de " + interval + " descendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }
  def randomArguments(): QuestionArguments =QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))
}

object Intervals extends QuestionGroup {
  override def toString: String = "Intervals"
  override def randomQuestion(): Question =  util.randomElement(List(ascendingInterval,descendingInterval))
}
