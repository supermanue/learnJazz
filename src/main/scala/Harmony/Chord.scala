package Harmony

class Chord (val intervals: List[Interval], val longName: String, val shortName: String){
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

  def majorChord = new Chord(List(
    new Interval(0, 0),
    new Interval(2, 0),
    new Interval(4, 0)), "mayor", "M")

  def minorChord = new Chord(List(
    new Interval(0, 0),
    new Interval(2, -1),
    new Interval(4, 0)), "menor", "m")

  def augmentedChord = new Chord(List(
    new Interval(0, 0),
    new Interval(2, 0),
    new Interval(4, 1)), "aumentado", "aug")

  def diminishedChord = new Chord(List(
    new Interval(0, 0),
    new Interval(2, -1),
    new Interval(4, -1)), "disminuido", "dim")

  def maj7Chord = new Chord(List(
    new Interval(0,0),
    new Interval(2,0),
    new Interval(4,0),
    new Interval(6,0)),"mayor 7", "maj7")

  def minor7Chord = new Chord(List(
    new Interval(0,0),
    new Interval(2,-1),
    new Interval(4,0),
    new Interval(6,-1)),"menor 7", "m7")

  def seventhChord = new Chord(List(
    new Interval(0,0),
    new Interval(2,0),
    new Interval(4,0),
    new Interval(6,-1)),"dominante", "7")

  def halfdimishedChord = new Chord(List(
    new Interval(0,0),
    new Interval(2,-1),
    new Interval(4,-1),
    new Interval(6,-1)),"semidisminuido", "m7b5")

  def dimishedCuatriadChord = new Chord(List(
    new Interval(0,0),
    new Interval(2,-1),
    new Interval(4,-1),
    new Interval(6,-2)),"disminuido", "O")

  def maj7plus5Chord = new Chord(List(
    new Interval(0,0),
    new Interval(2,0),
    new Interval(4,1),
    new Interval(6,0)),"mayor 7 sostenido 5", "maj7#5")

  def minorMaj7Chord = new Chord(List(
    new Interval(0,0),
    new Interval(2,-1),
    new Interval(4,0),
    new Interval(6,0)),"menor maj7", "mMaj7")

  def minor6Chord = new Chord(List(
    new Interval(0,0),
    new Interval(2,-1),
    new Interval(4,0),
    new Interval(5,0)),"menor 6", "m6")

}
