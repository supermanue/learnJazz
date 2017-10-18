package Util

import scala.util.Random

object util {
  def randomElement[T](elements:List[T]): T =
    elements(Random.nextInt(elements.size))


  def integerToRoman(n:Int): String = n match{
    case 1 => "I"
    case 2 => "II"
    case 3 => "III"
    case 4 => "IV"
    case 5 => "V"
    case 6 => "VI"
    case 7 => "VII"
    case _ => throw new IllegalArgumentException("Only values between 1 and 7")
  }

}
