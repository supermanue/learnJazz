package Harmony

import Util._

class Interval(val value: Int, val alterationVal: Int){
  val alteration = new Alteration(alterationVal)
  //assert(distance > 0, "Interval " + value  + ":" + alteration + " does not exist")

  def distance : Int = (value, alteration.value) match{
    case (0,0) => 0 //unisono
    case (1,-2) => 0 //segunda disminuida
    case (1,-1) => 1 //segunda menor
    case (1,0) => 2 //segunda mayor
    case (1,1) => 3 //segunda aumentada
    case (2,-2) => 2 //tercera disminuida
    case (2,-1) => 3 //tercera menor
    case (2,0) => 4 //tercera mayor
    case (2,1) => 5 //tercera aumentada
    case (3,-1) => 4  //cuarta disminuida
    case (3,0) => 5 //cuarta justa
    case (3,1) => 6 //cuarta aumentada
    case (4,-1) => 6 //quinta disminuida
    case (4,0) => 7 //quinta justa
    case (4,1) => 8 //quinta aumentada
    case (5,-2) => 7 // sexta disminuida
    case (5,-1) => 8 // sexta menor
    case (5,0) => 9 // sexta mayor
    case (5,1) => 10 // sexta aumentada
    case (6,-2) => 9 // septima disminuida
    case (6,-1) => 10 // septima menor
    case (6,0) => 11 // septima mayor
    case (6,1) => 12 // septima aumentada
    case (7,0) => 12 //octava
    case _ => -1 //TODO raise error here
  }

  //TODO second phase: english
  override def toString: String = (value, alteration.value) match{
    case (0,0) => "unisono"
    case (1,-2) => "segunda disminuida"
    case (1,-1) => "segunda menor"
    case (1,0) => "segunda mayor"
    case (1,1) => "segunda aumentada"
    case (2,-2) => "tercera disminuida"
    case (2,-1) => "tercera menor"
    case (2,0) => "tercera mayor"
    case (2,1) => "tercera aumentada"
    case (3,-1) => "cuarta disminuida"
    case (3,0) => "cuarta justa"
    case (3,1) => "cuarta aumentada"
    case (4,-1) => "quinta disminuida"
    case (4,0) => "quinta justa"
    case (4,1) => "quinta aumentada"
    case (5,-2) => "sexta disminuida"
    case (5,-1) => "sexta menor"
    case (5,0) => "sexta mayor"
    case (5,1) => "sexta aumentada"
    case (6,-2) => "septima disminuida"
    case (6,-1) => "septima menor"
    case (6,0) => "septima mayor"
    case (6,1) => "septima aumentada"
    case (7,0) => "octava"
    case _ => "intervalo desconocido" //TODO raise error here

  }

}

object IntervalGenerator {
  def possibleIntervals(): List[Interval] = List(
    new Interval(0,0),
    new Interval(1,-2),
    new Interval(1,-1),
    new Interval(1,0),
    new Interval(1,1),
    new Interval(2,-2),
    new Interval(2,-1),
    new Interval(2,0),
    new Interval(2,1),
    new Interval(3,-1),
    new Interval(3,0),
    new Interval(3,1),
    new Interval(4,-1),
    new Interval(4,0),
    new Interval(4,1),
    new Interval(5,-2),
    new Interval(5,-1),
    new Interval(5,0),
    new Interval(5,1),
    new Interval(6,-2),
    new Interval(6,-1),
    new Interval(6,0),
    new Interval(7,0))

}


