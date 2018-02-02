package Questions

import Harmony._
import Util.util

import scala.util.Random


object  keySignatureWithSharps extends Question {

  override def toString: String = "keySignatureWithSharps"

  def apply(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val (key, sharps) = Key.sharpsForPosition(pos - 1)
    val question = "¿Que tonalidad tiene " + (pos + 1) + "sostenidos?"
    val answer = key + " mayor: " + sharps
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}

object getSharpsForSignature  extends Question {

  override def toString: String = "getSharpsForSignature"

  def apply(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val (key, sharps) = Key.sharpsForPosition(pos - 1)
    val question = "¿Cuál es la armadura de " + key + " mayor?"
    val answer =  sharps
    (question, answer)
  }
  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}



object  keySignatureWithFlats extends Question {

  override def toString: String = "keySignatureWithFlats"

  def apply(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val (key, flats) = Key.flatsForPosition(pos - 1)
    val question = "¿Que tonalidad tiene " + (pos + 1) + "sostenidos?"
    val answer = key + " mayor: " + flats
    (question, answer)
  }

  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}

object getFlatsForSignature  extends Question {

  override def toString: String = "getFlatsForSignature"

  def apply(q: QuestionArguments): (String, String) = {
    val pos = 1 + q.number
    val (key, flats) = Key.flatsForPosition(pos - 1)
    val question = "¿Cuál es la armadura de " + key + " mayor?"
    val answer =  flats
    (question, answer)
  }
  override def randomArguments(): QuestionArguments = QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
}


object Keys extends QuestionGroup{
  override def toString: String = "Keys"
  override def randomQuestion(): Question =  util.randomElement(
    List(keySignatureWithSharps, getSharpsForSignature, keySignatureWithFlats, getFlatsForSignature))
}
