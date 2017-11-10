package Questions

import Harmony._

import scala.util.Random
import Util._

object TonalAreas extends QuestionGroup{

  def tonalArea(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val allTonalAreas = TonalArea(rootNote)

    val (tonalAreaName, tonalArea) = List(
      ("tónica", allTonalAreas.tonic),
      ("subdominante", allTonalAreas.subdominant),
      ("dominante", allTonalAreas.dominant))(q.number) //TODO 0-2

    val question="Qué acordes forman parte del area tonal " + tonalAreaName + " de " +  rootNote + " mayor?"
    val answer = tonalArea.map(note=>note._1 + "" + note._2).mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments =
    QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(3))


  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(tonalArea), tonalArea)))
}
