package Questions

import Harmony._
import Util._

import scala.util.Random


object Scales extends QuestionGroup{

  def modeNotes(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.intervals.map(interval => rootNote + interval)

    val question="Notas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def modeTensions(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solution = mode.tensions.map(interval => rootNote + interval)

    val question="Tensiones del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }

  def modeForbiddenNotes(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number) //TODO tamaño de esto?
    val solution = mode.forbiddenNodes.map(interval => rootNote + interval)

    val question="Notas prohibidas del modo " + rootNote + " " + mode
    val answer = solution.mkString(", ")
    (question, answer)
  }


  def modeHarmonization(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val mode = ScaleGenerator.modes()(q.number)
    val solutionNotes = mode.intervals.map(interval => rootNote + interval) zip mode.harmonization


    val question="Harmonización por cuatriadas del modo " + rootNote + " " + mode
    val answer = solutionNotes.map(a=>a._1 + "" + a._2).mkString(", ")
    (question, answer)
  }


  def scaleFromSecondaryDominant(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number
    val scales = ScaleGenerator.secondaryDominantScale(positionToReplace)

    val scaleNotes = for(mylist<-scales) yield mylist.intervals.map(interval => rootNote + interval)


    val question = "Escala a utilizar en la dominante secundaria V7/" + util.integerToRoman(positionToReplace+1) +
    " de la tonalidad de " + rootNote + " mayor"
    val answer = scaleNotes.map(myList => myList.mkString(", ")).mkString("; ")

    (question, answer)
  }

  def scaleFromTritoneSubstitution(q: QuestionArguments): (String, String) = {
    val rootNote = q.rootNote
    val positionToReplace = 1 + q.number

    val startScale = rootNote + ScaleGenerator.majorScale.intervals(positionToReplace) + Interval(1,-1)
    val scale = ScaleGenerator.lydianB9.intervals.map(interval => startScale + interval)

    val question = "Escala que usar en la sustitución por tritono subV7/" + util.integerToRoman(positionToReplace+1) +
    " en la escala de " + rootNote + " mayor."

    val answer = scale.mkString(", ")

    (question, answer)
  }


  override def questionGenerator (f: QuestionArguments=>(String, String)): QuestionArguments = {
    val modeNotesF = modeNotes _
    val modeTensionsF = modeTensions _
    val modeForbiddenNotesF = modeForbiddenNotes _
    val modeHarmonizationF = modeHarmonization _
    val scaleFromSecondaryDominantF = scaleFromSecondaryDominant _
    val scaleFromTritoneSubstitutionF = scaleFromTritoneSubstitution _


    f match {
      case `modeNotesF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
      case `modeTensionsF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
      case `modeForbiddenNotesF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),Random.nextInt(7))
      case `modeHarmonizationF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(7))
      case `scaleFromSecondaryDominantF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()), Random.nextInt(6))
      case `scaleFromTritoneSubstitutionF` => QuestionArguments(util.randomElement(NoteGenerator.possibleKeys()),util.randomElement(List(1,2,4)))

    }
  }

  override def randomQuestion(): Question =  util.randomElement(List(
    Question(questionGenerator(modeNotes), modeNotes),
    Question(questionGenerator(modeTensions), modeTensions),
    Question(questionGenerator(modeForbiddenNotes), modeForbiddenNotes),
    Question(questionGenerator(modeHarmonization), modeHarmonization),
    Question(questionGenerator(scaleFromSecondaryDominant), scaleFromSecondaryDominant),
    Question(questionGenerator(scaleFromTritoneSubstitution), scaleFromTritoneSubstitution)))

  }
