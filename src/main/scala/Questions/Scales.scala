package Questions

import Harmony._
import Util._

import scala.util.Random


object Scales extends QuestionGroup{

  def modeNotes(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.intervals.map(interval => rootNote + interval)

    val question="Notas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def modeTensions(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solution = mode.tensions.map(interval => rootNote + interval)

    val question="Tensiones del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def modeForbiddenNotes(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.forbiddenNodes.map(interval => rootNote + interval)

    val question="Notas prohibidas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }


  def modeHarmonization(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solutionNotes = mode.intervals.map(interval => rootNote + interval) zip mode.harmonization


    val question="Harmonización por cuatriadas del modo " + rootNote + " " + mode
    val answer = solutionNotes.map(a=>a._1 + "" + a._2).mkString(", ")
    (question, answer)
  }


  def scaleFromSecondaryDominant(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number
    val scales = ScaleGenerator.secondaryDominantScale(positionToReplace)

    val scaleNotes = for(mylist<-scales) yield mylist.intervals.map(interval => rootNote + interval)


    val question = "Escala a utilizar en la dominante secundaria V7/" + util.integerToRoman(positionToReplace+1) +
    " de la tonalidad de " + rootNote + " mayor"
    val answer = scaleNotes.map(myList => myList.mkString(", ")).mkString("; ")

    (question, answer)
  }

  def scaleFromTritoneSubstitution(q: Question): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number

    val startScale = rootNote + ScaleGenerator.majorScale.intervals(positionToReplace) + new Interval(1,-1)
    val scale = ScaleGenerator.lydianB9.intervals.map(interval => startScale + interval)

    val question = "Escala que usar en la sustitución por tritono subV7/" + util.integerToRoman(positionToReplace+1) +
    " en la escala de " + rootNote + " mayor."

    val answer = scale.mkString(", ")

    (question, answer)
  }


  override def questionGenerator (f: Question=>(String, String)): Question = f match {
    case modeNotes => new Question(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
    case modeTensions => new Question(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
    case modeForbiddenNotes => new Question(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
    case modeHarmonization => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
    case scaleFromSecondaryDominant => new Question(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
    case scaleFromTritoneSubstitution => new Question(util.randomElement(NoteGenerator.possibleKeys()),util.randomElement(List(1,2,4)))

  }

  override def randomQuestion(): (Question, Question => (String, String)) =  util.randomElement(List(
    (questionGenerator(modeNotes), modeNotes),
    (questionGenerator(modeTensions), modeTensions),
    (questionGenerator(modeForbiddenNotes), modeForbiddenNotes),
    (questionGenerator(modeHarmonization), modeHarmonization),
    (questionGenerator(scaleFromSecondaryDominant), scaleFromSecondaryDominant),
    (questionGenerator(scaleFromTritoneSubstitution), scaleFromTritoneSubstitution)))

  }
