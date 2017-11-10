package Questions

import Harmony._
import Util.util

import scala.util.Random

object Keys  extends QuestionGroup{
  def key_signature_with_flats(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val solution = Key.allSharps(pos -1)
    val question= "¿Que tonalidad tiene " + (pos +1) + "sostenidos?"
    val answer = solution + " mayor"
    (question, answer)

  }

  def get_flats_for_signature(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val note = Key.allSharps(pos -1)
    val flats = Key.sharps(pos)
    val question= "¿Cuál es la armadura de " + note + " mayor?"
    val answer = flats
    (question, answer)
  }

  //TODO lo mismo para bemoles cuando funcione


  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val key_signature_with_flatsF = key_signature_with_flats _
    val get_flats_for_signatureF = get_flats_for_signature _

    f match {
      case `key_signature_with_flatsF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
      case `get_flats_for_signatureF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(key_signature_with_flats), key_signature_with_flats),
    Question(questionGenerator(get_flats_for_signature), get_flats_for_signature)))
}
