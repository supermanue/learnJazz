package Questions

import Harmony._
import Util._
import scala.util.Random

object Triads extends QuestionGroup{
  def ascendingTriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval)

    val question = "triada " + triad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingTriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval).reverse

    val question = "triada " + triad.toString + " descentente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case ascendingTriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
    case descendingTriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(ascendingTriad), ascendingTriad),
    (questionGenerator(descendingTriad), descendingTriad)))
}


object CuatriadsEasy extends QuestionGroup{
  def ascendingCuatriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval)

    val question = "cuatriada " + cuatriad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingCuatriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

    val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case ascendingCuatriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
    case descendingCuatriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(ascendingCuatriad), ascendingCuatriad),
    (questionGenerator(descendingCuatriad), descendingCuatriad)))

}

object CuatriadsHard  extends QuestionGroup{
  def ascendingCuatriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval)

    val question = "cuatriada " + cuatriad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingCuatriad(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

    val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case ascendingCuatriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
    case descendingCuatriad => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(ascendingCuatriad), ascendingCuatriad),
    (questionGenerator(descendingCuatriad), descendingCuatriad)))
}

object CuatriadsInMajorScale  extends QuestionGroup{
  def cuatriadInScale(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val solution = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString +
      ScaleGenerator.majorScale.harmonization(posToReplace)


    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la cuatriada del grado " +
      util.integerToRoman(posToReplace + 1) + "?"
    val answer = solution
    (question, answer)
  }

  def analyze_chord_in_Scale(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val chord = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString
    val cuatriad = ScaleGenerator.majorScale.harmonization(posToReplace)


    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la función del acorde " + chord + cuatriad + "?"
    val answer = util.integerToRoman(posToReplace + 1) + cuatriad
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case cuatriadInScale => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
    case analyze_chord_in_Scale => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))

  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(cuatriadInScale), cuatriadInScale),
    (questionGenerator(analyze_chord_in_Scale), analyze_chord_in_Scale)))

  //TODO same with minor scales
  //this has to be separated so can be chosen by the user (maybe only knows major scale...)
}


