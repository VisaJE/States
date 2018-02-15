package states

import scala.collection.mutable.Map



class Laitos(val hinta: Vector[Tuote], val työt: Map[Työ,Int]) {
  
  private var omistaja: Option[String] = None
  
  
  def onOmistettu = {
    if (omistaja != None) true
    else false
  }
  
  
  def osta(ostaja: String) = if (this.onOmistettu) false
  else {
    omistaja = Some(ostaja)
    true
  }
}


// Sekä hinta että tuottavuus määritetään pelikohtaisesti
class Pelto(hinta: Vector[Tuote], tuottavuus: Int) extends Laitos(hinta, Map(new Viljely -> tuottavuus))