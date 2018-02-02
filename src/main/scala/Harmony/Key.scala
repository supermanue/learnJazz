package Harmony


object Key {
  def allFlats = List (("", "C"), ("B", "F"), ("E", "Bb"), ("A", "Eb"), ("D", "Ab"), ("G", "Db"), ("C", "Gb"), ("F", "Cb"))
  def allSharps = List (("", "C"), ("F","G") , ("C","D"), ("G","A"), ("D","E"), ("A","B"), ("E","F#"), ("B","C#"))

  def sharpsForPosition(pos: Int): (String, String) = (allSharps(pos)._2, allSharps.slice(1,pos+1).map{a => a._1 +"#"}.mkString(" "))
  def flatsForPosition(pos: Int): (String, String) = (allFlats(pos)._2, allFlats.slice(1,pos+1).map{a => a._1 +"b"}.mkString(" "))

}
