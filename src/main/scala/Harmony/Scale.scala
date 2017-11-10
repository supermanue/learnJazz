package Harmony

case class Scale (intervals: List[Interval],
             tensions: List[Interval],
             forbiddenNodes: List[Interval],
             harmonization: List[Chord],
             name: String){

  override def toString: String = name.toString


}




object ScaleGenerator{
  def modes():List[Scale] = List(
    ionianMode, dorianMode, phrygianMode, lydianMode,
    mixolidianMode, aeolianMode, locrianMode)

  def secondaryDominantScale(elem: Int):List[Scale] = elem match{
    case 1 => List(seventhScaleB13,seventhScaleB9M9B13)
    case 2 => List(seventhScaleB9B13, seventhScaleB9M9)
    case 3 => List(seventhScale)
    case 4 =>List(seventhScale,seventhScaleB9M9B13)
    case 5 => List(seventhScaleB9B13, seventhScaleB9M9)
    case _ => List()

  }


  //TODO generar todos los modos a partir del jónico, empezando desde un punto distinto
  def ionianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,0),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,0)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(3,0)
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

  def dorianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(3,0)
    ),
    List(
      Interval(5,0)
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


  def phrygianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,-1),
      Interval(6,-1)),
    List(
      Interval(3,0)
    ),
    List(
      Interval(1,-1),
      Interval(5,-1)
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


  def lydianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,0),
      Interval(3,1),
      Interval(4,0),
      Interval(5,0),
      Interval(6,0)),
    List(
      Interval(1,0),
      Interval(3,1),
      Interval(5,0)
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


  def mixolidianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,0),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(3,0)
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


  def aeolianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,-1),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(3,0)
    ),
    List(
      Interval(5,-1)
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


  def locrianMode = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,-1),
      Interval(5,-1),
      Interval(6,-1)),
    List(
      Interval(3,0),
      Interval(5,-1)
    ),
    List(
      Interval(1,-1)
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

  def majorScale :Scale  = ionianMode
  def naturalMinorScale :Scale = aeolianMode
  def armonicMinor :Scale = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,-1),
      Interval(6,0)),
    List(
      Interval(1,0),
      Interval(3,0)
    ),
    List(
      Interval(5,-1)
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

  def melodicMinor = Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,0)),
    List(
      Interval(1,0),
      Interval(3,0)
    ),
    List(
      Interval(5,0)
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


  def seventhScale :Scale =mixolidianMode

  //TODO modifica todo o solo esto?
  def seventhScaleB9 = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(2,0),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(4,0)
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

  def seventhScaleB13 =Scale (
    List(
      Interval(0,0),
      Interval(1,0),
      Interval(1,1),
      Interval(2,-1),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(4,0)
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
  def seventhScaleB9B13 = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(2,0),
      Interval(3,0),
      Interval(4,-1),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(4,0)
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

  def seventhScaleB9M9 = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(1,1),
      Interval(2,0),
      Interval(3,0),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(4,0)
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


  def seventhScaleB9M9B13 = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(1,1),
      Interval(2,0),
      Interval(3,-1),
      Interval(4,0),
      Interval(5,0),
      Interval(6,-1)),
    List(
      Interval(1,0),
      Interval(5,0)
    ),
    List(
      Interval(4,0)
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

  def lydianB9 = Scale (
    List(
      Interval(0,0),
      Interval(1,-1),
      Interval(2,0),
      Interval(3,1),
      Interval(4,0),
      Interval(5,0),
      Interval(6,0)),
    List(
      Interval(1,0),
      Interval(3,1),
      Interval(5,0)
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


