package Harmony


object Key {
  def allSharps = List ("F", "C", "G", "D", "A", "E", "B")
  def allFlats = List ("B", "E", "A", "D", "G", "C", "F")

  def sharps(pos: Int): String = allSharps.slice(0,pos).map{a => a+"#"}.mkString(" ")
  def flats(pos: Int): String = allFlats.slice(0,pos).map{a => a+"b"}.mkString(" ")


}
