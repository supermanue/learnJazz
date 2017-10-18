package Harmony

class Scale (val intervals: List[Interval],
             val tensions: List[Interval],
             val forbiddenNodes: List[Interval],
             val harmonization: List[Chord],
             val name: String){

  override def toString: String = name.toString


}




object ScaleGenerator{
  def modes():List[Scale] = List(
    ionianMode, dorianMode, phrygianMode, lydianMode,
    mixolidianMode, aeolianMode, locrianMode)

  def secondaryDominantScale(elem: Int) = elem match{
    case 1 => List(seventhScaleB13,seventhScaleB9M9B13)
    case 2 => List(seventhScaleB9B13, seventhScaleB9M9)
    case 3 => List(seventhScale)
    case 4 =>List(seventhScale,seventhScaleB9M9B13)
    case 5 => List(seventhScaleB9B13, seventhScaleB9M9)
    case _ => List()

  }


  //TODO generar todos los modos a partir del jónico, empezando desde un punto distinto
  def ionianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,0),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,0)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(3,0)
    ),
    List(
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord
    ),
    "jónico")

  def dorianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(3,0)
    ),
    List(
      new Interval(5,0)
    ),
    List(
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord
    ),
    "dórico")


  def phrygianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,-1),
      new Interval(6,-1)),
    List(
      new Interval(3,0)
    ),
    List(
      new Interval(1,-1),
      new Interval(5,-1)
    ),
    List(
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord
    ),
    "frigio")


  def lydianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,0),
      new Interval(3,1),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,0)),
    List(
      new Interval(1,0),
      new Interval(3,1),
      new Interval(5,0)
    ),
    List(
    ),
    List(
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord
    ),
    "lidio")


  def mixolidianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,0),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(3,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "mixolidio")


  def aeolianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,-1),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(3,0)
    ),
    List(
      new Interval(5,-1)
    ),
    List(
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord
    ),
    "eólico")


  def locrianMode = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,-1),
      new Interval(5,-1),
      new Interval(6,-1)),
    List(
      new Interval(3,0),
      new Interval(5,-1)
    ),
    List(
      new Interval(1,-1)
    ),
    List(
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord
    ),
    "locrio")

  def majorScale = ionianMode
  def naturalMinorScale = aeolianMode
  def armonicMinor = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,-1),
      new Interval(6,0)),
    List(
      new Interval(1,0),
      new Interval(3,0)
    ),
    List(
      new Interval(5,-1)
    ),
    List(
      ChordGenerator.minorMaj7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.dimishedCuatriadChord
    ),
    "menor armónico")

  def melodicMinor = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,0)),
    List(
      new Interval(1,0),
      new Interval(3,0)
    ),
    List(
      new Interval(5,0)
    ),
    List(
      ChordGenerator.minorMaj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.halfdimishedChord
    ),
    "menor melódico")


  def seventhScale=mixolidianMode

  //TODO modifica todo o solo esto?
  def seventhScaleB9 = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(2,0),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(4,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "septima b9")

  def seventhScaleB13 =new Scale (
    List(
      new Interval(0,0),
      new Interval(1,0),
      new Interval(1,1),
      new Interval(2,-1),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(4,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "septima b13")
  def seventhScaleB9B13 = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(2,0),
      new Interval(3,0),
      new Interval(4,-1),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(4,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "septima b9 b13")

  def seventhScaleB9M9 = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(1,1),
      new Interval(2,0),
      new Interval(3,0),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(4,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "septima b9 #9")


  def seventhScaleB9M9B13 = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(1,1),
      new Interval(2,0),
      new Interval(3,-1),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,-1)),
    List(
      new Interval(1,0),
      new Interval(5,0)
    ),
    List(
      new Interval(4,0)
    ),
    List(
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.maj7Chord
    ),
    "septima b9 #9 b13")

  def lydianB9 = new Scale (
    List(
      new Interval(0,0),
      new Interval(1,-1),
      new Interval(2,0),
      new Interval(3,1),
      new Interval(4,0),
      new Interval(5,0),
      new Interval(6,0)),
    List(
      new Interval(1,0),
      new Interval(3,1),
      new Interval(5,0)
    ),
    List(
    ),
    List(
      ChordGenerator.maj7Chord ,
      ChordGenerator.seventhChord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.halfdimishedChord ,
      ChordGenerator.maj7Chord ,
      ChordGenerator.minor7Chord ,
      ChordGenerator.minor7Chord
    ),
    "lidio b9")


}


