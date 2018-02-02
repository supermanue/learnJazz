package Questions

import Harmony._
import Util._
import scala.util.Random


object  ascendingTriad extends Question {

  override def toString: String = "ascendingTriad"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval)

    val question = "triada " + triad.toString + " de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
}

object descendingTriad  extends Question {

  override def toString: String = "descendingTriad"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val triad = ChordGenerator.possibleTriads()(q.number)
    val solution = triad.intervals.map(interval => rootNote + interval).reverse

    val question = "triada " + triad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
    }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
}

object Triads extends QuestionGroup{
  override def toString: String = "Triads"
  override def randomQuestion(): Question =  util.randomElement(List(ascendingTriad, descendingTriad))
}


object  ascendingCuatriad extends Question {

  override def toString: String = "ascendingCuatriad"

  def apply(q: QuestionArguments): (String, String) = {
  val rootNote = q.rootNote
  val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
  val solution = cuatriad.intervals.map(interval => rootNote + interval)

  val question = "cuatriada " + cuatriad.toString + " de " + rootNote
  val answer = solution.mkString(", ")
  (question, answer)

  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
}


object descendingCuatriad extends Question {

  override def toString: String = "descendingCuatriad"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
    val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

    val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
    val answer = solution.mkString(", ")
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(4))
}

object CuatriadsEasy extends QuestionGroup{
  override def toString: String = "CuatriadsEasy"
  override def randomQuestion(): Question =  util.randomElement(List( ascendingCuatriad, descendingCuatriad))
}



object ascendingCuatriadHard  extends Question {

override def toString: String = "ascendingCuatriadHard"

def apply(q: QuestionArguments): (String, String) = {
  val rootNote = q.rootNote
  val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
  val solution = cuatriad.intervals.map(interval => rootNote + interval)

  val question = "cuatriada " + cuatriad.toString + " de " + rootNote
  val answer = solution.mkString(", ")
  (question, answer)

}

override def randomArguments(): QuestionArguments =QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
}



object descendingCuatriadHard  extends Question {

override def toString: String = "descendingCuatriadHard"

def apply(q: QuestionArguments): (String, String) = {
  val rootNote = q.rootNote
  val cuatriad = ChordGenerator.possibleCuatriads()(q.number) //TODO: tamaño de esto?
  val solution = cuatriad.intervals.map(interval => rootNote + interval).reverse

  val question = "cuatriada " + cuatriad.toString + " descendente de " + rootNote
  val answer = solution.mkString(", ")
  (question, answer)
}

override def randomArguments(): QuestionArguments =QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(8))
}

object CuatriadsHard  extends QuestionGroup{
  override def toString: String = "CuatriadsHard"
  override def randomQuestion(): Question =  util.randomElement(List(ascendingCuatriadHard, descendingCuatriadHard))
}


object cuatriadInMajorScale   extends Question {

  override def toString: String = "cuatriadInMajorScale"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val solution = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString +
    ScaleGenerator.majorScale.harmonization(posToReplace)
    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la cuatriada del grado " +
    util.integerToRoman(posToReplace + 1) + "?"
    val answer = solution
    (question, answer)

  }

  override def randomArguments(): QuestionArguments =QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}


object analyzeChordInMajorScale  extends Question {

  override def toString: String = "analyzeChordInMajorScale"

  def apply(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val posToReplace = q.number
    val chord = (rootNote + ScaleGenerator.majorScale.intervals(posToReplace)).toString
    val cuatriad = ScaleGenerator.majorScale.harmonization(posToReplace)


    val question = "En la tonalidad de " + rootNote + " mayor, ¿Cual es la función del acorde " + chord + cuatriad + "?"
    val answer = util.integerToRoman(posToReplace + 1) + cuatriad
    (question, answer)
  }

  override def randomArguments(): QuestionArguments =QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}


object CuatriadsInMajorScale  extends QuestionGroup{
  override def toString: String = "CuatriadsInMajorScale"
  override def randomQuestion(): Question =  util.randomElement(List(cuatriadInMajorScale, analyzeChordInMajorScale))
}

//TODO same with minor scales
//this has to be separated so can be chosen by the user (maybe only knows major scale...)

