package Harmony

case class Chord (intervals: List[Interval], longName: String, shortName: String){
  override def toString: String = shortName.toString

}


object ChordGenerator {
  def possibleTriads(): List[Chord] = List(
    majorChord, minorChord, augmentedChord, diminishedChord)

  def possibleCuatriadsEasy(): List[Chord] = List(
    maj7Chord, minor7Chord, seventhChord, halfdimishedChord)

  def possibleCuatriadsHard(): List[Chord] = List(
    dimishedCuatriadChord, maj7plus5Chord, minorMaj7Chord, minor6Chord)

  def possibleCuatriads(): List[Chord] = possibleCuatriadsEasy() ++ possibleCuatriadsHard()

  def majorChord = Chord(List(
    Interval(0, 0),
    Interval(2, 0),
    Interval(4, 0)), "mayor", "M")

  def minorChord = Chord(List(
    Interval(0, 0),
    Interval(2, -1),
    Interval(4, 0)), "menor", "m")

  def augmentedChord = Chord(List(
    Interval(0, 0),
    Interval(2, 0),
    Interval(4, 1)), "aumentado", "aug")

  def diminishedChord = Chord(List(
    Interval(0, 0),
    Interval(2, -1),
    Interval(4, -1)), "disminuido", "dim")

  def maj7Chord = Chord(List(
    Interval(0,0),
    Interval(2,0),
    Interval(4,0),
    Interval(6,0)),"mayor 7", "maj7")

  def minor7Chord = Chord(List(
    Interval(0,0),
    Interval(2,-1),
    Interval(4,0),
    Interval(6,-1)),"menor 7", "m7")

  def seventhChord = Chord(List(
    Interval(0,0),
    Interval(2,0),
    Interval(4,0),
    Interval(6,-1)),"dominante", "7")

  def halfdimishedChord = Chord(List(
    Interval(0,0),
    Interval(2,-1),
    Interval(4,-1),
    Interval(6,-1)),"semidisminuido", "m7b5")

  def dimishedCuatriadChord = Chord(List(
    Interval(0,0),
    Interval(2,-1),
    Interval(4,-1),
    Interval(6,-2)),"disminuido", "O")

  def maj7plus5Chord = Chord(List(
    Interval(0,0),
    Interval(2,0),
    Interval(4,1),
    Interval(6,0)),"mayor 7 sostenido 5", "maj7#5")

  def minorMaj7Chord = Chord(List(
    Interval(0,0),
    Interval(2,-1),
    Interval(4,0),
    Interval(6,0)),"menor maj7", "mMaj7")

  def minor6Chord = Chord(List(
    Interval(0,0),
    Interval(2,-1),
    Interval(4,0),
    Interval(5,0)),"menor 6", "m6")

}
