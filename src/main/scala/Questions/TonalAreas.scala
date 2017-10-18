package Questions

import Harmony._

import scala.util.Random
import Util._

object TonalAreas extends QuestionGroup{

  def tonalArea(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val allTonalAreas = new TonalArea(rootNote)

    val (tonalAreaName, tonalArea) = List(
      ("tónica", allTonalAreas.tonic),
      ("subdominante", allTonalAreas.subdominant),
      ("dominante", allTonalAreas.dominant))(q.number) //TODO 0-2

    val question="Qué acordes forman parte del area tonal " + tonalAreaName + " de " +  rootNote + " mayor?"
    val answer = tonalArea.map(note=>note._1 + "" + note._2).mkString(", ")
    (question, answer)
  }

  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case tonalArea => new Question(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(3))

  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(tonalArea), tonalArea)))
}
