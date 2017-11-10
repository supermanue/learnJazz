package Questions

import Harmony._
import Util._
import scala.util.Random

object Triads extends QuestionGroup{
  def ascendingTriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval)

    val question = "triada " + triad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingTriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval).reverse

    val question = "triada " + triad.toString + " descentente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val ascendingTriadF = ascendingTriad _
    val descendingTriadF = descendingTriad _
    f match {
      case `ascendingTriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
      case `descendingTriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(ascendingTriad), ascendingTriad),
    Question(questionGenerator(descendingTriad), descendingTriad)))
}


object CuatriadsEasy extends QuestionGroup{
  def ascendingCuatriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval)

    val question = "cuatriada " + cuatriad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingCuatriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

    val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val ascendingCuatriadF =ascendingCuatriad _
    val descendingCuatriadF = descendingCuatriad _
    f match {
      case `ascendingCuatriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
      case `descendingCuatriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(ascendingCuatriad), ascendingCuatriad),
    Question(questionGenerator(descendingCuatriad), descendingCuatriad)))

}

object CuatriadsHard  extends QuestionGroup{
  def ascendingCuatriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval)

    val question = "cuatriada " + cuatriad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def descendingCuatriad(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

    val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val ascendingCuatriadF = ascendingCuatriad _
    val descendingCuatriadF = descendingCuatriad _
    f match {
      case `ascendingCuatriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
      case `descendingCuatriadF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(ascendingCuatriad), ascendingCuatriad),
    Question(questionGenerator(descendingCuatriad), descendingCuatriad)))
}

object CuatriadsInMajorScale  extends QuestionGroup{
  def cuatriadInScale(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val solution = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString +
      ScaleGenerator.majorScale.harmonization(posToReplace)


    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la cuatriada del grado " +
      util.integerToRoman(posToReplace + 1) + "?"
    val answer = solution
    (question, answer)
  }

  def analyzeChordInScale(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val chord = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString
    val cuatriad = ScaleGenerator.majorScale.harmonization(posToReplace)


    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la función del acorde " + chord + cuatriad + "?"
    val answer = util.integerToRoman(posToReplace + 1) + cuatriad
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val cuatriadInScaleF = cuatriadInScale _
    val analyzeChordInScaleF = analyzeChordInScale _
    f match {
      case `cuatriadInScaleF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
      case `analyzeChordInScaleF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))

    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(cuatriadInScale), cuatriadInScale),
    Question(questionGenerator(analyzeChordInScale), analyzeChordInScale)))

  //TODO same with minor scales
  //this has to be separated so can be chosen by the user (maybe only knows major scale...)
}


