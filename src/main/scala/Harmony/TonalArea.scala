package Harmony

case class TonalArea(root:Note) {
  private val scale = ScaleGenerator.majorScale
  private val intervals = scale.intervals
  private val harmonization = scale.harmonization

  def tonic: List[(Note, Chord)] = List(
    (root, harmonization.head),
  (root + intervals(2),harmonization(2)),
  (root + intervals(5), harmonization(5)))

  def subdominant: List[(Note, Chord)] = List(
    (root + intervals(1),harmonization(1)),
    (root + intervals(3), harmonization(3)))


  def dominant: List[(Note, Chord)] = List(
    (root + intervals(4),harmonization(4)),
    (root + intervals(6), harmonization(6)))

}
