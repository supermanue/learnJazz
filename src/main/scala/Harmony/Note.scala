package Harmony

import java.lang.Math.floorMod

case class Note(value:Int, alterationVal: Int){
  val alteration = Alteration(alterationVal)
  val numNotes = 7 //there are 7 notes
  val relativeDistances: Vector[Int] =Vector[Int](2, 2, 1, 2, 2, 2, 1) //distance from C to D, D to E...

  assert(value < numNotes, "notes are between 0 and 6")

  def valueToString: String = value match{
    case 0 => "C"
    case 1 => "D"
    case 2 => "E"
    case 3 => "F"
    case 4 => "G"
    case 5 => "A"
    case 6 => "B"
  }
  override def toString: String = valueToString ++ alteration.toString

  def + (interval:Interval): Note = {
    val faceValue = (value+ interval.value) % numNotes

    var faceDistance = 0
    for (i <- 0 until interval.value) {
      faceDistance += relativeDistances((value + i) % numNotes)
    }

    Note(faceValue, interval.distance - faceDistance + alteration.value)
  }

  def - (interval:Interval): Note = {
    val faceValue = floorMod(value- interval.value,numNotes)
    var faceDistance = 0

    for (i <- 0 until interval.value) {
      faceDistance += relativeDistances(floorMod(value - i -1, numNotes))
    }
    Note(faceValue, faceDistance - interval.distance + alteration.value)

  }
  override def equals(that: Any): Boolean = that match {
    case that: Note => (value == that.value) && (alterationVal == that.alterationVal)
    case _ => false
  }

}

object NoteGenerator{
  def possibleNotes ():List[Note] = {
    val myList = for {
      baseN <- 0 to 6
      baseA <- -1 to 1
    } yield Note (baseN, baseA)
    myList.toList
  }

  def possibleKeys (): List[Note] =
    List (
      Note(0,0), //C: C, C#
      Note(0,1),
      Note(1,0), //D: D, Db, D#
      Note(1,-1),
      Note(1,1),
      Note(2,0), //E: E, Eb
      Note(2,-1),
      Note(3,0), //F: F, F#
      Note(3,1),
      Note(4,0), //G: G, G#
      Note(4,1),
      Note(5,0), //A: A, Ab
      Note(5,-1),
      Note(6,0),  //B: B, Bb
      Note(6,-1))

}

