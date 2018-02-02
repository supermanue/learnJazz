import Harmony._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner



@RunWith(classOf[JUnitRunner])
class ScaleSuite extends FunSuite {
  val C = Note(0,0)

  def getfullScaleInfo(myScale: Scale):(String, String, String) = (
    (myScale.intervals.map(a => C + a )  zip
      myScale.harmonization).map(a=>a._1.toString + a._2.toString).mkString(" "),
    myScale.tensions.map(a => C + a ).mkString(" "),
    myScale.forbiddenNodes.map(a => C + a ).mkString(" "))

  test ("ionian mode"){
    val CionianMode  = getfullScaleInfo(ScaleGenerator.ionianMode)
    assert (CionianMode._1=="Cmaj7 Dm7 Em7 Fmaj7 G7 Am7 Bm7b5", "ionian mode is not well defined")
    assert (CionianMode._2=="D A", "ionian mode forbidden notes are not well defined")
    assert (CionianMode._3=="F", "ionian mode forbidden notes are not well defined")
  }

  test ("dorian mode "){
    val CdorianMode  = getfullScaleInfo(ScaleGenerator.dorianMode)
    assert (CdorianMode._1=="Cm7 Dm7 Ebmaj7 F7 Gm7 Am7b5 Bbmaj7", "dorian mode is not well defined")
    assert (CdorianMode._2=="D F", "dorian mode forbidden notes are not well defined")
    assert (CdorianMode._3=="A", "dorian mode forbidden notes are not well defined")
  }

  test ("phrygian mode"){
    val CphrygianMode  = getfullScaleInfo(ScaleGenerator.phrygianMode)
    assert (CphrygianMode._1=="Cm7 Dbmaj7 Eb7 Fm7 Gm7b5 Abmaj7 Bbm7", "phrygian mode is not well defined")
    assert (CphrygianMode._2=="F", "phrygian mode forbidden notes are not well defined")
    assert (CphrygianMode._3=="Db Ab", "phrygian mode forbidden notes are not well defined")
  }

  test ("lydian mode"){
    val ClydianMode  = getfullScaleInfo(ScaleGenerator.lydianMode)
    assert (ClydianMode._1=="Cmaj7 D7 Em7 F#m7b5 Gmaj7 Am7 Bm7", "lydian mode is not well defined")
    assert (ClydianMode._2=="D F# A", "lydian mode forbidden notes are not well defined")
    assert (ClydianMode._3=="", "lydian mode forbidden notes are not well defined")
  }

  test ("mixolidian mode"){
    val CmixolidianMode  = getfullScaleInfo(ScaleGenerator.mixolidianMode)
    assert (CmixolidianMode._1=="C7 Dm7 Em7b5 Fmaj7 Gm7 Am7 Bbmaj7", "mixolidian mode is not well defined")
    assert (CmixolidianMode._2=="D A", "mixolidian mode forbidden notes are not well defined")
    assert (CmixolidianMode._3=="F", "mixolidian mode forbidden notes are not well defined")
  }

  test ("aeolian mode"){
    val CaeolianMode  = getfullScaleInfo(ScaleGenerator.aeolianMode)
    assert (CaeolianMode._1=="Cm7 Dm7b5 Ebmaj7 Fm7 Gm7 Abmaj7 Bb7", "aeolian mode is not well defined")
    assert (CaeolianMode._2=="D F", "aeolian mode forbidden notes are not well defined")
    assert (CaeolianMode._3=="Ab", "aeolian mode forbidden notes are not well defined")
  }

  test ("locrian mode"){
    val ClocrianMode  = getfullScaleInfo(ScaleGenerator.locrianMode)
    assert (ClocrianMode._1=="Cm7b5 Dbmaj7 Ebm7 Fm7 Gbmaj7 Ab7 Bbm7", "locrian mode is not well defined")
    assert (ClocrianMode._2=="F Ab", "locrian mode forbidden notes are not well defined")
    assert (ClocrianMode._3=="Db", "locrian mode forbidden notes are not well defined")
  }

  test ("armonic minor"){
    val CarmonicMinor  = getfullScaleInfo(ScaleGenerator.armonicMinor)
    assert (CarmonicMinor._1=="CmMaj7 Dm7b5 Ebmaj7 Fm7 G7 Abmaj7 BO", "armonic Minor mode is not well defined")
    assert (CarmonicMinor._2=="D F", "armonic Minor mode forbidden notes are not well defined")
    assert (CarmonicMinor._3=="Ab", "armonic Minor mode forbidden notes are not well defined")
  }

  test ("melodic minor"){
    val CmelodicMinor  = getfullScaleInfo(ScaleGenerator.melodicMinor)
    assert (CmelodicMinor._1=="CmMaj7 Dm7 Ebmaj7 F7 G7 Am7b5 Bm7b5", "melodic Minor mode is not well defined")
    assert (CmelodicMinor._2=="D F", "melodic Minor mode forbidden notes are not well defined")
    assert (CmelodicMinor._3=="A", "melodic Minor mode forbidden notes are not well defined")
  }

  test ("seventh b9"){
    val CmixolidianMode  = getfullScaleInfo(ScaleGenerator.mixolidianMode)
    assert (CmixolidianMode._1=="C7 Dm7 Em7b5 Fmaj7 Gm7 Am7 Bbmaj7", "mixolidian mode is not well defined")
    assert (CmixolidianMode._2=="D A", "mixolidian mode forbidden notes are not well defined")
    assert (CmixolidianMode._3=="F", "mixolidian mode forbidden notes are not well defined")
  }


}


@RunWith(classOf[JUnitRunner])
class TonalAreaSuite extends FunSuite {

  test ("tonal areas ") {
    val rootNote = Note(0,0)
    val allAreas = TonalArea(rootNote)
    val tonic = allAreas.tonic.map(a=>a._1).mkString(" ")
    val subdominant = allAreas.subdominant.map(a=>a._1).mkString(" ")
    val dominant = allAreas.dominant.map(a=>a._1).mkString(" ")

    assert (tonic=="C E A", "tonic tonal area is not well defined")
    assert (subdominant=="D F", "subdominant tonal area is not well defined")
    assert (dominant=="G B", "dominant tonal area is not well defined")
  }

}

@RunWith(classOf[JUnitRunner])
class NoteSuite extends FunSuite {

  test ("equality"){
    val A = Note(0,0)
    val B = Note (0,0)
    assert (A==B, "Equality of notes is not correctly defined")
  }

  test("adding intervals") {
    val C = Note(0,0)
    val allNotes = List(
      C,
      C + Interval(1,0),
      C + Interval(2,0),
      C + Interval(3,0),
      C + Interval(4,0),
      C + Interval(5,0),
      C + Interval(6,0)
    ).mkString(" ")
    assert(allNotes == "C D E F G A B", "interval adding is not correct")
  }

  test("adding and subtracting intervals") {
    var simetry = true
      for (baseN <- 0 to 6)
        for (baseA <- -1 to 1)
          for (intB <- 0 to 7){
            val interval = Interval(intB, 0)
            val baseNote = Note (baseN, baseA)
            if ((baseNote - interval + interval) != baseNote)
              simetry=false

          }
      assert(simetry, "simetry in adding and substracting intervals is not correct")
    }

}



@RunWith(classOf[JUnitRunner])
class KeySuite extends FunSuite {

  test("sharp order") {
    val allSharps = Key.sharpsForPosition(Key.allSharps.size-1)._2
    assert(allSharps == "F# C# G# D# A# E# B#", "Sharps are not in the correct order")
  }
  test("no sharps") {
    val noSharps = Key.sharpsForPosition(0)._1
    assert(noSharps == "C", "no sharps is wrong")
  }

  test("one sharp") {
    val oneSharp = Key.sharpsForPosition(1)._1
    assert(oneSharp == "G", "one sharp is wrong")
  }

  test("flat order") {
    val allFlats = Key.flatsForPosition(Key.allFlats.size-1)._2
    assert(allFlats == "Bb Eb Ab Db Gb Cb Fb", "Flats are not in the correct order")
  }
  test("no flats") {
    val noFlats = Key.flatsForPosition(0)._1
    assert(noFlats == "C", "no flats is wrong")
  }

  test("one flat") {
    val oneFlat = Key.flatsForPosition(1)._1
    assert(oneFlat == "F", "one flat is wrong")
  }



}


@RunWith(classOf[JUnitRunner])
class ChordSuite extends FunSuite {
  val C = Note(0,0)
  test("major triad") {
    val chord = ChordGenerator.majorChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C E G", "Major triad is not well constructed")
  }

  test("minor triad") {
    val chord = ChordGenerator.minorChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb G", "Minor triad is not well constructed")
  }

  test("augmented triad") {
    val chord = ChordGenerator.augmentedChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C E G#", "Augmented triad is not well constructed")
  }

  test("dimished triad") {
    val chord = ChordGenerator.diminishedChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb Gb", "Dimished triad is not well constructed")
  }

  test("maj7 cuatriad") {
    val chord = ChordGenerator.maj7Chord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C E G B", "Major triad is not well constructed")
  }

  test("seventh cuatriad") {
    val chord = ChordGenerator.seventhChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C E G Bb", "Seventh cuatriad is not well constructed")
  }

  test("minor seventh cuatriad") {
    val chord = ChordGenerator.minor7Chord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb G Bb", "Minor seventh cuatriad is not well constructed")
  }

  test("minor maj7 cuatriad") {
    val chord = ChordGenerator.minorMaj7Chord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb G B", "Minor maj7 cuatriad is not well constructed")
  }

  test("minor6  cuatriad") {
    val chord = ChordGenerator.minor6Chord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb G A", "Minor 6 cuatriad is not well constructed")
  }

  test("half dimished cuatriad") {
    val chord = ChordGenerator.halfdimishedChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb Gb Bb", "half dimished cuatriad is not well constructed")
  }

  test(" dimished cuatriad") {
    val chord = ChordGenerator.dimishedCuatriadChord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C Eb Gb Bbb", " dimished cuatriad is not well constructed")
  }

  test("maj7 #5 cuatriad") {
    val chord = ChordGenerator.maj7plus5Chord
    val solution = chord.intervals.map(interval => C+interval).mkString(" ")
    assert(solution == "C E G# B", "maj7#5 cuatriad is not well constructed")
  }

}