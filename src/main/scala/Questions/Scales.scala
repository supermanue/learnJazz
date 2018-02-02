package Questions

import Harmony._
import Util._

import scala.util.Random



object modeNotes  extends Question {

  override def toString: String = "modeNotes"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.intervals.map(interval => rootNote + interval)

    val question = "Notas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}

object  modeTensions extends Question {

  override def toString: String = "modeTensions"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solution = mode.tensions.map(interval => rootNote + interval)

    val question = "Tensiones del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}

object modeForbiddenNotes  extends Question {

  override def toString: String = "modeForbiddenNotes"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.forbiddenNodes.map(interval => rootNote + interval)

    val question = "Notas prohibidas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))

}

object modeHarmonization  extends Question {

  override def toString: String = "modeHarmonization"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solutionNotes = mode.intervals.map(interval => rootNote + interval) zip mode.harmonization


    val question = "Harmonización por cuatriadas del modo " + rootNote + " " + mode
    val answer = solutionNotes.map(a => a._1 + "" + a._2).mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))

}

object scaleFromSecondaryDominant  extends Question {

  override def toString: String = "scaleFromSecondaryDominant"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number
    val scales = ScaleGenerator.secondaryDominantScale(positionToReplace)

    val scaleNotes = for (mylist <- scales) yield mylist.intervals.map(interval => rootNote + interval)


    val question = "Escala a utilizar en la dominante secundaria V7/" + util.integerToRoman(positionToReplace + 1) +
      " de la tonalidad de " + rootNote + " mayor"
    val answer = scaleNotes.map(myList => myList.mkString(", ")).mkString("; ")

    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
}


object scaleFromTritoneSubstitution  extends Question {

  override def toString: String = "scaleFromTritoneSubstitution"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number

    val startScale = rootNote + ScaleGenerator.majorScale.intervals(positionToReplace) + Interval(1, -1)
    val scale = ScaleGenerator.lydianB9.intervals.map(interval => startScale + interval)

    val question = "Escala que usar en la sustitución por tritono subV7/" + util.integerToRoman(positionToReplace + 1) +
      " en la escala de " + rootNote + " mayor."

    val answer = scale.mkString(", ")

    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), util.randomElement(List(1, 2, 4)))
}



object Scales extends QuestionGroup{
  override def toString: String = "Scales"
  override def randomQuestion(): Question =  util.randomElement(List(modeNotes, modeTensions, modeForbiddenNotes,
    modeHarmonization, scaleFromSecondaryDominant, scaleFromTritoneSubstitution))

  }
