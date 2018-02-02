package Questions

import Harmony._
import Util.util

import scala.util.Random


object  twoFives extends Question {

  override def toString: String = "twoFives"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val two = rootNote + Interval(1, 0)
    val five = rootNote + Interval(4, 0)

    val question = "ii-V de " + rootNote
    val answer = two + "m7 / " + five + "7"
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), 0)
}


object analyzeTwoFives  extends Question {

  override def toString: String = "analyzeTwoFives"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val two = rootNote + Interval(1, 0)
    val five = rootNote + Interval(4, 0)

    val question = "En la tonalidad de " + rootNote + ", cual es la función de los acordes " + two + "m7 / " + five + "7?"
    val answer = "ii-V"
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), 0)
}

object dominant  extends Question {

  override def toString: String = "dominant"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val dominant = rootNote + Interval(4, 0)

    val question = "De quién es dominante (o dominante secundaria) " + dominant + "7?"
    val answer = rootNote.toString
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), 0)
}

object analyzeDominants extends Question {

  override def toString: String = "analyzeDominants"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val position = q.number //TODO q entre 0 y 5

    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + Interval(4, 0)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función del acorde " + dominant + "7?"
    val answer =
      if (position == 0) "V7"
      else "V7/" + util.integerToRoman(position + 1)
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
}


object dominantFromNote  extends Question {

  override def toString: String = "dominantFromNote"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val position = q.number //TODO q entre 0 y  5
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + Interval(4, 0)

    val question = "En la tonalidad de " + rootNote + " mayor, quién es el V7 del " + (position + 1) + "?"
    val answer = dominant + "7"
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
}

object Dominants extends QuestionGroup {
  override def toString: String = "Dominants"
  override def randomQuestion(): Question =  util.randomElement(List( twoFives, analyzeTwoFives, dominant, analyzeDominants,dominantFromNote))
}


object tritone  extends Question {

  override def toString: String = "tritone"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    //val chordsInScale = ScaleGenerator.majorScale.harmonization

    val position = q.number
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + Interval(4, 0)
    val tritone_substitution = noteToReplace + Interval(1, -1)

    val question = "En la tonalidad de " + rootNote + " mayor, de quién es sustituto por tritono " + tritone_substitution + "7 y en qué grado?"
    val answer = dominant.toString + "7, grado " + util.integerToRoman(position + 1)
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
}
object analizeTritone  extends Question {

  override def toString: String = "analizeTritone"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals

    val position = q.number //everything except IV7 and bVII7

    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + Interval(4, 0)
    val tritone_substitution = noteToReplace + Interval(1, -1)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función del acorde " + tritone_substitution + "7?"
    val answer = "subV7/" + util.integerToRoman(position + 1) + " (sustituto de " + dominant + ")"
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), util.randomElement(List(0, 1, 2, 4)))
}


object twoTritone  extends Question {

  override def toString: String = "twoTritone"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val two = rootNote + Interval(1, 0)
    val tritone_substitution = rootNote + Interval(1, -1)

    val question = "Cuál es el ii-subV7 que resuelve en " + rootNote + "?"
    val answer = two + "m7 / " + tritone_substitution + "7"

    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), 0)
}

object analyzeTwoTritone  extends Question {

  override def toString: String = "analyzeTwoTritone"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    //val chordsInScale = ScaleGenerator.majorScale.harmonization

    val position = q.number
    val noteToReplace = rootNote + intervalsInScale(position)
    val two = noteToReplace + Interval(1, 0)
    val tritone_substitution = noteToReplace + Interval(1, -1)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función de los acordes " + two + "m7 " + tritone_substitution + "7?"
    val answer = "ii-subV7"
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}

object twoFiveDivisionInTritone  extends Question {

  override def toString: String = "twoFiveDivisionInTritone"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val two = rootNote - Interval(2, -1) //descending minor third from tritone
    val tritone_substitution = rootNote + Interval(1, -1) //tritone substitution of the V7: minor second

    val question = "Cual es la visión en ii-V del subV7 que resuelve en " + rootNote + "7?"
    val answer = two + "m7" + " / " + tritone_substitution + "7"
    (question, answer)
  }

  //TODO esto igual casca, la lié al copiar/pegar y me lo he inventado
  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}


object Tritones extends QuestionGroup{
  override def toString: String = "Tritones"
   override def randomQuestion(): Question =  util.randomElement(List(tritone, analizeTritone,
     twoTritone, analyzeTwoTritone, twoFiveDivisionInTritone))
}
