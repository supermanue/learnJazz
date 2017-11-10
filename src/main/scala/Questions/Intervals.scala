package Questions

import Harmony._
import Util.util

import scala.util.Random

object Intervals extends QuestionGroup {


  def ascendingInterval(q: QuestionArguments):(String, String)={
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote + interval

    val question="intervalo de " + interval + " ascendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }

  def descendingInterval(q: QuestionArguments):(String, String)={
    val rootNote = q.rootNote
    val interval = IntervalGenerator.possibleIntervals()(q.number)
    val solution = rootNote - interval

    val question="intervalo de " + interval + " descendente de " + rootNote
    val answer = solution.toString
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val ascendingIntervalF= ascendingInterval _
    val descendingIntervalF=descendingInterval _
    f match {
      case `ascendingIntervalF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))
      case `descendingIntervalF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(23))
    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(ascendingInterval), ascendingInterval),
    Question(questionGenerator(descendingInterval), descendingInterval)))



}
