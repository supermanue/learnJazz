package Harmony



case class Alteration(value: Int){
  assert((-4 < value) &&(value  < 4), "alteration must be between -3 and 3")

  override def toString: String = value match{
    case -3 => "bbb"
    case -2 => "bb"
    case -1 => "b"
    case 0 => ""
    case 1 => "#"
    case 2 => "##"
    case 3 => "###"

  }
}