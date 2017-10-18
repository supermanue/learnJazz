package Questions

import Harmony._
import Util.util

import scala.util.Random

object Dominants extends QuestionGroup {

  def twoFives(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val two = rootNote + new Interval(1,0)
    val five = rootNote + new Interval(4,0)

    val question = "ii-V de " + rootNote
    val answer = two + "m7 / " + five + "7"
    (question, answer)
  }

  def analyzeTwoFives(q: Question):(String, String) = {
    val rootNote =q.rootNote
    val two = rootNote + new Interval(1,0)
    val five = rootNote + new Interval(4,0)

    val question = "En la tonalidad de " + rootNote + ", cual es la función de los acordes " + two + "m7 / " + five + "7?"
    val answer="ii-V"
    (question, answer)
  }

  def dominant(q: Question):(String, String) = {
    val rootNote = q.rootNote
    val dominant = rootNote + new Interval(4,0)

    val question = "De quién es dominante (o dominante secundaria) " + dominant + "7?"
    val answer = rootNote.toString
    (question, answer)
  }

  def analyze_dominants(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val position = q.number //TODO q entre 0 y 5

    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + new Interval(4,0)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función del acorde " + dominant + "7?"
    val answer =
      if (position==0) "V7"
      else "V7/" + util.integerToRoman(position+1)
    (question, answer)
  }


  def dominant_from_note(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val position = q.number //TODO q entre 0 y  5
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + new Interval(4,0)

    val question = "En la tonalidad de " + rootNote + " mayor, quién es el V7 del " + (position +1)  + "?"
    val answer = dominant + "7"
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case twoFives => new Question(util.randomElement(NoteGenerator.possibleKeys()),0)
    case analyzeTwoFives => new Question(util.randomElement(NoteGenerator.possibleKeys()), 0)
    case dominant => new Question(util.randomElement(NoteGenerator.possibleKeys()), 0)
    case analyze_dominants => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
    case dominant_from_note => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))

  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(twoFives), twoFives),
    (questionGenerator(analyzeTwoFives), analyzeTwoFives),
    (questionGenerator(dominant), dominant),
    (questionGenerator(analyze_dominants), analyze_dominants),
    (questionGenerator(dominant_from_note), dominant_from_note)))
}

object Tritones extends QuestionGroup{
  def tritone(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val chordsInScale = ScaleGenerator.majorScale.harmonization

    val position = q.number
    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + new Interval(4,0)
    val tritone_substitution = noteToReplace + new Interval(1,-1)

    val question = "En la tonalidad de " + rootNote + " mayor, de quién es sustituto por tritono " + tritone_substitution + "7 y en qué grado?"
    val answer = dominant.toString + "7, grado " + util.integerToRoman(position +1)
    (question, answer)
  }

  def analize_tritone(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals

    val position = q.number //everything except IV7 and bVII7

    val noteToReplace = rootNote + intervalsInScale(position)
    val dominant = noteToReplace + new Interval(4,0)
    val tritone_substitution = noteToReplace + new Interval(1,-1)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función del acorde " + tritone_substitution + "7?"
    val answer = "subV7/" + util.integerToRoman(position+1) + " (sustituto de " + dominant + ")"
    (question, answer)
  }

  def two_tritone(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val two = rootNote + new Interval(1,0)
    val tritone_substitution = rootNote + new Interval(1,-1)

    val question = "Cuál es el ii-subV7 que resuelve en " + rootNote + "?"
    val answer = two + "m7 / " + tritone_substitution + "7"

    (question, answer)
  }

  def analyze_two_tritone(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val intervalsInScale = ScaleGenerator.majorScale.intervals
    val chordsInScale = ScaleGenerator.majorScale.harmonization

    val position = q.number
    val noteToReplace = rootNote + intervalsInScale(position)
    val two = noteToReplace + new Interval(1,0)
    val tritone_substitution = noteToReplace + new Interval(1,-1)

    val question = "En la tonalidad de " + rootNote + " mayor, cuál es la función de los acordes " + two + "m7 " + tritone_substitution+ "7?"
    val answer = "ii-subV7"
    (question, answer)
  }

  def two_five_division_in_tritone(q: Question):(String,String) = {
    val rootNote = q.rootNote
    val two = rootNote - new Interval(2,-1) //descending minor third from tritone
    val tritone_substitution = rootNote + new Interval(1,-1) //tritone substitution of the V7: minor second

    val question = "Cual es la visión en ii-V del subV7 que resuelve en " + rootNote + "7?"
    val answer = two + "m7" + " / " + tritone_substitution + "7"
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case tritone => new Question(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(6))
    case analize_tritone => new Question(util.randomElement(NoteGenerator.possibleKeys()), util.randomElement(List(0,1,2,4)))
    case two_tritone => new Question(util.randomElement(NoteGenerator.possibleKeys()), 0)
    case analyze_two_tritone => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
    case two_five_division_in_tritone => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))

  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(tritone), tritone),
    (questionGenerator(analize_tritone), analize_tritone),
    (questionGenerator(two_tritone), two_tritone),
    (questionGenerator(analyze_two_tritone), analyze_two_tritone),
    (questionGenerator(two_five_division_in_tritone), two_five_division_in_tritone)))

}
