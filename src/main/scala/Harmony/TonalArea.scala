package Harmony

class TonalArea(val root:Note) {
  val scale = ScaleGenerator.majorScale
  val intervals = scale.intervals
  val harmonization = scale.harmonization

  def tonic: List[(Note, Chord)] = List(
    (root, harmonization(0)),
  ((root + intervals(2)),harmonization(2)),
  (root + intervals(5), harmonization(5)))

  def subdominant: List[(Note, Chord)] = List(
    ((root + intervals(1)),harmonization(1)),
    (root + intervals(3), harmonization(3)))


  def dominant: List[(Note, Chord)] = List(
    ((root + intervals(4)),harmonization(4)),
    (root + intervals(6), harmonization(6)))

}
